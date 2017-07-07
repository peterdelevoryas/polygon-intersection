extern crate render1;
extern crate gl;
extern crate cgmath;

use render1::Object;
use cgmath::Matrix4;
use cgmath::SquareMatrix;
use cgmath::Vector2;
use cgmath::Vector3;
use cgmath::InnerSpace;

#[derive(Debug, Copy, Clone)]
struct AABB {
    min: Vector2<f32>,
    max: Vector2<f32>,
}

impl AABB {
    fn object(&self, color: &'static str) -> Object<Vec<[f32; 2]>> {
        let p0 = [self.min.x, self.min.y];
        let p1 = [self.max.x, self.min.y];
        let p2 = [self.max.x, self.max.y];
        let p3 = [self.min.x, self.max.y];
        let vertices = vec![p0, p1, p2, p3, p0];
        Object::outline(vertices, color)
    }

    fn segments(&self) -> Vec<Segment> {
        let p0: Vector2<f32> = [self.min.x, self.min.y].into();
        let p1: Vector2<f32> = [self.max.x, self.min.y].into();
        let p2: Vector2<f32> = [self.max.x, self.max.y].into();
        let p3: Vector2<f32> = [self.min.x, self.max.y].into();

        vec![
            Segment { x0: p0, x: p1 - p0 },
            Segment { x0: p0, x: p3 - p0 },
            Segment { x0: p2, x: p1 - p2 },
            Segment { x0: p2, x: p3 - p2 },
        ]
    }
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

impl Intersection {
    fn object(&self, color: &'static str) -> Option<Object<Vec<[f32; 2]>>> {
        match *self {
            Intersection::None => None,
            Intersection::Point(p) => Some(Object::point(p.into(), color)),
            Intersection::Segment(s) => Some(Object::segment(vec![s.x0.into(), (s.x0 + s.x).into()], color)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Interval {
    left: Endpoint,
    right: Endpoint,
}

enum IntervalIntersection {
    None,
    Point(f32),
    Interval(Interval),
}

fn point_or_interval_intersection(left: Endpoint, right: Endpoint) -> IntervalIntersection {
    if left == right && left.inclusive && right.inclusive {
        IntervalIntersection::Point(left.value)
    } else {
        IntervalIntersection::Interval(Interval { left, right })
    }
}

impl Interval {
    fn intersection(self, rhs: Interval) -> IntervalIntersection {
        let a = self.left;
        let b = self.right;
        let x = rhs.left;
        let y = rhs.right;

        if a <= b && b <= x && x <= y {
            IntervalIntersection::None
        } else if a <= x && x <= b && b <= y {
            point_or_interval_intersection(x, b)
        } else if a <= x && x <= y && y <= b {
            point_or_interval_intersection(x, y)
        } else if x <= a && a <= b && b <= y {
            point_or_interval_intersection(a, b)
        } else if x <= a && a <= y && y <= b {
            point_or_interval_intersection(a, y)
        } else if x <= y && y <= a && a <= b {
            point_or_interval_intersection(y, a)
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Endpoint {
    value: f32,
    inclusive: bool,
}

impl Endpoint {
    fn inclusive(value: f32) -> Endpoint {
        Endpoint { value, inclusive: true }
    }
    fn exclusive(value: f32) -> Endpoint {
        Endpoint { value, inclusive: false }
    }
}

use std::cmp::PartialEq;
use std::cmp::PartialOrd;
use std::cmp::Ordering;
impl PartialEq for Endpoint {
    fn eq(&self, other: &Endpoint) -> bool {
        self.value == other.value
    }
}
impl PartialOrd for Endpoint {
    fn partial_cmp(&self, other: &Endpoint) -> Option<Ordering> {
        if self.value < other.value {
            Some(Ordering::Less)
        } else if self.value == other.value {
            Some(Ordering::Equal)
        } else if self.value > other.value {
            Some(Ordering::Greater)
        } else {
            None
        }
    }
}

trait Cross2 {
    type Scalar;
    fn cross(self, rhs: Self) -> Self::Scalar;
}

impl Cross2 for Vector2<f32> {
    type Scalar = f32;
    fn cross(self, rhs: Self) -> f32 {
        self.x * rhs.y - self.y * rhs.x
    }
}

fn ray_segment_intersection(ray: Ray, segment: Segment) -> Intersection {
    let a0 = ray.x0;
    let a = ray.direction;
    let b0 = segment.x0;
    let b = segment.x;
    let t_a = (b0 - a0).cross(b) / a.cross(b);
    let t_b = (b0 - a0).cross(a) / a.cross(b);
    let zero_to_inf = Interval {
        left: Endpoint::inclusive(0.0),
        right: Endpoint::exclusive(std::f32::INFINITY)
    };

    if a.cross(b) == 0.0 && (b0 - a0).cross(a) == 0.0 {
        let t0 = (b0 - a0).dot(a) / a.dot(a);
        let t1 = t0 + b.dot(a) / a.dot(a);
        let (t0, t1) = if a.dot(b) > 0.0 { (t0, t1) } else { (t1, t0) };
        let t0_to_t1 = Interval {
            left: Endpoint::inclusive(t0),
            right: Endpoint::inclusive(t1),
        };
        match t0_to_t1.intersection(zero_to_inf) {
            IntervalIntersection::None => Intersection::None,
            IntervalIntersection::Point(p) => {
                Intersection::Point(a0 + a * p)
            }
            IntervalIntersection::Interval(i) => {
                let p0 = a0 + a * i.left.value;
                let p1 = a0 + a * i.right.value;
                Intersection::Segment(Segment {
                    x0: p0,
                    x: p1 - p0,
                })
            }
        }
    } else if a.cross(b) == 0.0 && (b0 - a0).cross(a) != 0.0 {
        Intersection::None
    } else if a.cross(b) != 0.0 && t_a >= 0.0 && t_b >= 0.0 {
        Intersection::Point(a0 + a * t_a)
    } else {
        Intersection::None
    }
}

fn main() {
    let aabb = AABB {
        min: [-0.5, -0.5].into(),
        max: [1.0, 1.0].into(),
    };

    let ray = Ray {
        x0: [0.25, 0.25].into(),
        direction: [-1.0, 0.0].into(),
    };

    let mut objects = vec![aabb.object("purple"), ray.object("red")];
    for segment in aabb.segments() {
        let intersection = ray_segment_intersection(ray, segment);
        if let Some(object) = intersection.object("pink") {
            objects.push(object);
        }
    }

    render1::render1(&objects);
}
