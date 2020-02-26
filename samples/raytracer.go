// Raytracer demo in Go

package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"time"
)

type Vec [3]float64

func (u Vec) add(v Vec) Vec         { return Vec{u[0] + v[0], u[1] + v[1], u[2] + v[2]} }
func (u Vec) sub(v Vec) Vec         { return Vec{u[0] - v[0], u[1] - v[1], u[2] - v[2]} }
func (u Vec) dot(v Vec) float64     { return u[0]*v[0] + u[1]*v[1] + u[2]*v[2] }
func (u Vec) mul(a float64) Vec     { return Vec{u[0] * a, u[1] * a, u[2] * a} }
func (u Vec) elementwise(v Vec) Vec { return Vec{u[0] * v[0], u[1] * v[1], u[2] * v[2]} }
func norm(v Vec) float64            { return math.Sqrt(v.dot(v)) }
func normalize(v Vec) Vec           { return v.mul(1.0 / norm(v)) }
func randn() Vec                    { return Vec{rand.Float64() - 0.5, rand.Float64() - 0.5, rand.Float64() - 0.5} }

type Color = Vec

type Ray struct {
	origin, dir Vec
}

type GenericBody struct {
	center      Vec
	color       Color
	diffuseness float64
	isLamp      bool
}

func (b *GenericBody) get() *GenericBody { return b }

func (b *GenericBody) lambertFactor(lambert float64) float64 {
	return 1.0 - (1.0-lambert)*b.diffuseness
}

type Body interface {
	get() *GenericBody
	intersect(ray *Ray) (found bool, point, normal Vec)
}

type Box struct {
	GenericBody
	halfsize Vec
}

func within(x, y, xmin, ymin, xmax, ymax float64) bool {
	return (x > xmin) && (x < xmax) && (y > ymin) && (y < ymax)
}

func (b *Box) intersect(ray *Ray) (found bool, point, normal Vec) {

	var intersectFace = func(i, j, k int) bool {

		if math.Abs(ray.dir[k]) > 1e-9 {
			side := 1.0
			if ray.dir[k] > 0.0 {
				side = -1.0
			}

			if factor := (b.center[k] + side*b.halfsize[k] - ray.origin[k]) / ray.dir[k]; factor > 0.1 {
				point = ray.origin.add(ray.dir.mul(factor))
				if within(
					point[i], point[j],
					b.center[i]-b.halfsize[i], b.center[j]-b.halfsize[j],
					b.center[i]+b.halfsize[i], b.center[j]+b.halfsize[j]) {

					normal[i] = 0
					normal[j] = 0
					normal[k] = side
					return true
				}
			}
		}
		return false
	}

	return intersectFace(0, 1, 2) || intersectFace(2, 0, 1) || intersectFace(1, 2, 0), point, normal
}

type Sphere struct {
	GenericBody
	radius float64
}

func (s *Sphere) intersect(ray *Ray) (found bool, point, normal Vec) {
	displacement := s.center.sub(ray.origin)
	proj := displacement.dot(ray.dir)
	discr := s.radius*s.radius + proj*proj - displacement.dot(displacement)

	if discr > 0 {
		factor := proj - math.Sqrt(discr)
		if factor > 0.1 {
			point := ray.origin.add(ray.dir.mul(factor))
			normal := (point.sub(s.center)).mul(1.0 / s.radius)
			return true, point, normal
		}
	}

	return false, point, normal
}

type Scene struct {
	ambientColor Color
	body         []Body
}

func (sc *Scene) trace(ray *Ray, depth int) Color {
	if depth > 3 {
		return sc.ambientColor
	}

	// Find nearest intersection
	bestDist := 1e9
	bestIndex := -1
	var bestPoint, bestNormal Vec

	for i, b := range sc.body {

		if found, point, normal := b.intersect(ray); found {
			dist := norm(point.sub(ray.origin))
			if dist < bestDist {
				bestDist = dist
				bestIndex = i
				bestPoint = point
				bestNormal = normal
			}
		}
	}

	// Reflect rays
	if bestIndex >= 0 {
		bestBody := sc.body[bestIndex].get()

		if bestBody.isLamp {
			return bestBody.get().color
		}

		specularDir := ray.dir.sub(bestNormal.mul(2.0 * (ray.dir.dot(bestNormal))))
		diffuseDir := normalize(specularDir.add(randn().mul(2.0 * bestBody.diffuseness)))

		lambert := diffuseDir.dot(bestNormal)
		if lambert < 0 {
			diffuseDir = diffuseDir.sub(bestNormal.mul(2.0 * lambert))
			lambert = -lambert
		}

		diffuseRay := Ray{bestPoint, diffuseDir}

		return sc.trace(&diffuseRay, depth+1).mul(bestBody.lambertFactor(lambert)).elementwise(bestBody.color)
	}

	return sc.ambientColor
}

func main() {
	// Define scene
	scene := Scene{Color{0.2, 0.2, 0.2}, nil}

	scene.body = append(
		scene.body,
		&Box{
			GenericBody{
				Vec{500, -100, 1200},
				Color{0.4, 0.7, 1.0},
				0.1,
				false},
			Vec{400, 600, 300}.mul(0.5)})

	scene.body = append(
		scene.body,
		&Box{
			GenericBody{
				Vec{550, 210, 1100},
				Color{0.9, 1.0, 0.6},
				0.3,
				false},
			Vec{1000, 20, 1000}.mul(0.5)})

	scene.body = append(
		scene.body,
		&Sphere{
			GenericBody{
				Vec{600, 0, 700},
				Color{1.0, 0.4, 0.6},
				0.2,
				false},
			200})

	scene.body = append(
		scene.body,
		&Sphere{
			GenericBody{
				Vec{330, 150, 700},
				Color{1.0, 1.0, 0.3},
				0.15,
				false},
			50})

	// Define light
	scene.body = append(
		scene.body,
		&Sphere{
			GenericBody{
				Vec{500, -1000, -700},
				Color{1.0, 1.0, 1.0},
				1.0,
				true},
			800})

	// Define eye
	pos := Vec{0, 0, 0}
	const azimuth = 30.0 * math.Pi / 180.0
	sinAz, cosAz := math.Sin(azimuth), math.Cos(azimuth)
	const width, height = 640, 480
	const focal = 500
	const antialiasing = 1.0

	var rays int
	fmt.Printf("Raytracer demo\n\n")
	fmt.Printf("Rays per pixel (recommended 1 to 100): ")
	fmt.Scanf("%d\n", &rays)
	fmt.Println()

	// Open output file
	const fileName = "scene.ppm"
	f, err := os.Create(fileName)
	if err != nil {
		panic(err)
	}

	fmt.Fprintf(f, "P3\n%d %d\n255\n", width, height)

	// Render scene
	startTime := time.Now()

	for i := 0; i < height; i++ {
		for j := 0; j < width; j++ {
			dir := Vec{float64(j - width/2), float64(i - height/2), float64(focal)}

			rotDir := Vec{
				dir[0]*cosAz + dir[2]*sinAz,
				dir[1],
				-dir[0]*sinAz + dir[2]*cosAz}

			color := Color{0, 0, 0}
			for r := 0; r < rays; r++ {
				randomDir := rotDir.add(randn().mul(antialiasing))
				ray := Ray{pos, normalize(randomDir)}
				color = color.add(scene.trace(&ray, 0))
			}
			color = color.mul(255.0 / float64(rays))

			fmt.Fprintf(f, "%d %d %d ", byte(color[0]), byte(color[1]), byte(color[2]))
		}
		fmt.Fprintln(f)
		fmt.Printf("%3d/%3d\n", i+1, height)
	}

	endTime := time.Now()

	f.Close()

	fmt.Printf("\nRendering time = %v\n", endTime.Sub(startTime))
	fmt.Println("Done. See " + fileName)
	fmt.Scanf("\n")

}
