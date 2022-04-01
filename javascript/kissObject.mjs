class KiSSObject extends EventTarget {
  constructor (mark, positions) {
    super()

    this.mark = mark
    this.id = mark
    this.currentSet = 0
    this.positions = positions
    this.cels = []

    return this
  }

  update (currentSet) {
    this.currentSet = currentSet
    for (let i = 0; i < this.cels.length; i++) {
      this.cels[i].update(currentSet)
    }
  }

  get position () {
    return this.positions[this.currentSet]
  }

  get fixed () {
    return this.cels.some(c => c.fix > 0)
  }

  setPosition (x, y) {
    this.positions[this.currentSet].x = x
    this.positions[this.currentSet].y = y
  }

  altmap () {
    this.cels.forEach(cel => { cel.altmap() })
  }

  map () {
    this.cels.forEach(cel => cel.map())
  }

  move (dx, dy) {
    this.positions[this.currentSet].x += dx
    this.positions[this.currentSet].y += dy
  }

  setTransparency (alpha) {
    this.cels.forEach(cel => cel.setTransparency(alpha))
  }

  unmap () {
    this.cels.forEach(cel => cel.unmap())
  }
}

export { KiSSObject }
