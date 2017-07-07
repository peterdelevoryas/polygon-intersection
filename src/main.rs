extern crate render1;
extern crate gl;
extern crate cgmath;

use render1::Object;
use cgmath::Matrix4;
use cgmath::SquareMatrix;
use cgmath::Vector2;
use cgmath::Vector3;

#[derive(Debug, Copy, Clone)]
struct AABB {
    min: Vector2<f32>,
    max: Vector2<f32>,
}

#[derive(Debug, Copy, Clone)]
struct Segment {
    x0: Vector2<f32>,
    x: Vector2<f32>,
}

#[derive(Debug, Copy, Clone)]
struct Ray {
    x0: Vector2<f32>,
    direction: Vector2<f32>,
}

impl Ray {
    fn object(&self, color: &'static str) -> Object<Vec<[f32; 2]>> {
        Object::segment(vec![self.x0.into(), (self.x0 + self.direction * 1_000_000_000.0).into()], color)
    }
}

#[derive(Debug, Copy, Clone)]
enum Intersection {
    None,
    Point(Vector2<f32>),
    Segment(Segment),
}

fn ray_segment_intersection(ray: Ray, segment: Segment) -> Intersection {
    unimplemented!()
}

fn main() {
    let mut triangle = Object::outline(vec![
        [ 0.0,  0.5],
        [ 0.5, -0.5],
        [-0.5, -0.5],
        [ 0.0,  0.5],
    ], "red");
    triangle.model = Matrix4::from_translation(Vector3::new(0.0, 0.2, 0.0));

    let square = Object::outline(vec![
        [-0.5, -0.5],
        [-0.5,  0.5],
        [ 0.5,  0.5],
        [ 0.5, -0.5],
        [-0.5, -0.5],
    ], "blue");

    let dot = Object::point([0.0, 0.0], "black");
    let ray = Ray { x0: [0.1, 0.1].into(), direction: [2.0, 1.0].into() }.object("pink");
    println!("{:?}", ray);

    let objects = &[triangle, square, dot, ray];
    render1::render1(objects);
}
