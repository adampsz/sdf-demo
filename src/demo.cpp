#define GLM_ENABLE_EXPERIMENTAL

#include <bitset>
#include <cstdio>
#include <cstdlib>

#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/ext.hpp>
#include <glm/glm.hpp>

#include "shader.hpp"

class Camera {
  glm::mat4 view, proj;

public:
  void resize(int width, int height) {
    double aspect = width / (double)height;
    proj = glm::perspective(glm::radians(60.0), aspect, 0.1, 100.0);
  }

  void move(glm::vec3 position) {
    view = glm::lookAt(position, glm::vec3(0.0), glm::vec3(0.0, 1.0, 0.0));
  }

  glm::mat4 transform() { return glm::inverse(proj * view); }
};

class Shader {
  GLint program;

  struct {
    GLuint transform, time, display, scene;
  } uniforms;

  GLuint load(const char *code, GLenum type) {
    GLuint shader = glCreateShader(type);
    glShaderSource(shader, 1, &code, NULL);
    glCompileShader(shader);

    GLint length;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);

    if (length > 0) {
      char msg[length + 1];
      glGetShaderInfoLog(shader, length, NULL, msg);
      fprintf(stderr, "%s\n", msg);
    }

    return shader;
  }

public:
  Shader() : Shader(VERT, FRAG){};

  Shader(const char *vertex, const char *fragment) {
    program = glCreateProgram();
    GLuint vert = load(vertex, GL_VERTEX_SHADER);
    GLuint frag = load(fragment, GL_FRAGMENT_SHADER);

    glAttachShader(program, vert);
    glAttachShader(program, frag);

    glLinkProgram(program);

    glDetachShader(program, vert);
    glDetachShader(program, frag);
    glDeleteShader(vert);
    glDeleteShader(frag);

    uniforms.transform = glGetUniformLocation(program, "transform");
    uniforms.time = glGetUniformLocation(program, "time");
    uniforms.display = glGetUniformLocation(program, "display");
    uniforms.scene = glGetUniformLocation(program, "scene");
  }

  void bind(const glm::mat4 transform, int display, int scene) {
    glUseProgram(program);
    glUniformMatrix4fv(uniforms.transform, 1, GL_FALSE, &transform[0][0]);
    glUniform1f(uniforms.time, glfwGetTime());
    glUniform1i(uniforms.display, display);
    glUniform1i(uniforms.scene, scene);
  }

  ~Shader() { glDeleteProgram(program); }
};

class Screen {
  GLFWwindow *window;
  GLuint vertices;
  Shader shader;
  std::bitset<256> keys;

  static void framebuffer_size_callback(GLFWwindow *, int width, int height) {
    glViewport(0, 0, width, height);
  }

  GLFWwindow *open() {
    // Quick fix, please ignore
    unsetenv("WAYLAND_DISPLAY");
    putenv((char *)"XDG_SESSION_TYPE=x11");

    assert(glfwInit());

    glfwWindowHint(GLFW_SAMPLES, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    window = glfwCreateWindow(1024, 768, "SDF Demo", NULL, NULL);
    assert(window);

    glfwSetInputMode(window, GLFW_STICKY_KEYS, GLFW_TRUE);
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);
    glfwMakeContextCurrent(window);
    assert(glewInit() == GLEW_NO_ERROR);

    return window;
  }

public:
  Screen() : window(open()), shader(Shader()) {
    glGenVertexArrays(1, &vertices);
    glBindVertexArray(vertices);
    glEnableVertexAttribArray(0);
  }

  void size(int &width, int &height) {
    glfwGetWindowSize(window, &width, &height);
  }

  void mouse(double &x, double &y) {
    glfwGetCursorPos(window, &x, &y);
    y = y > 1 ? y : 1;
  }

  bool opened() {
    return glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS &&
           glfwWindowShouldClose(window) == 0;
  }

  bool pressed(int key) {
    bool prev = keys[key];
    keys[key] = glfwGetKey(window, key) == GLFW_PRESS;
    return !prev && keys[key];
  }

  void draw(const glm::mat4 transform, int display, int scene) {
    glClear(GL_COLOR_BUFFER_BIT);
    glBindFramebuffer(GL_FRAMEBUFFER, 1);
    shader.bind(transform, display, scene);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  ~Screen() {
    glDeleteVertexArrays(1, &vertices);
    glfwTerminate();
  }
};

int main() {
  Screen screen;
  Camera camera;
  int display = 0, scene = 0;

  camera.move(glm::vec3(2.0));

  while (screen.opened()) {
    int width, height;
    double mx, my;

    screen.size(width, height);
    screen.mouse(mx, my);

    glm::vec2 eye = 1.0f - glm::vec2(my / height, mx / width);

    camera.resize(width, height);
    camera.move(glm::euclidean(eye * glm::vec2(M_PI / 2, M_PI * 2)) * 30.0f);

    if (screen.pressed(GLFW_KEY_D))
      display = (display + 1) % 9;

    if (screen.pressed(GLFW_KEY_S))
      scene = (scene + 1) % 5;

    screen.draw(camera.transform(), display, scene);
  }

  return 0;
}
