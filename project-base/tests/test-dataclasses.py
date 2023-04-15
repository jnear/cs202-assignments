class Point:
  x: int
  y: int

def add_point(self: Point, other: Point) -> Point:
    return Point(self.x + other.x, self.y + other.y)

p1 = Point(1, 2)
p2 = Point(3, 4)
p3 = add_point(p1, p2)
print(p3.x)
print(p3.y)
