# SDF Demo

A simple SDF demo, based on the work of [Iñigo Quilez](https://iquilezles.org/).

## Building

Requirements: `glew`, `glfw3`, `glm`, build using `cmake`.

```sh
cd build
cmake ..
make
./demo
```

## Scenes (switch with <kbd>S</kbd>)

- Simple scene with few primitives
  ![Screenshot](./media/screenshot-1.png)
- Metaballs
  ![Screenshot](./media/screenshot-2.png)
- Menger sponge
  ![Screenshot](./media/screenshot-3.png)
- [Instytut Informatyki Uniwersytetu Wrocławskiego](https://maps.app.goo.gl/zLTDbTrNz4TE6iaj9)
  ![Screenshot](./media/screenshot-4.png)
- Instytut Informatyki Uniwersytetu Wrocławskiego (repeated)
  ![Screenshot](./media/screenshot-5.png)

## Displayed information (switch with <kbd>D</kbd>)

- Combined scene
- Vertical intersection
- Depth
- World position
- Normal vector
- Shading
- Shadows
- Ambient occlusion
- Number of evaluations
