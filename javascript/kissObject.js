class KiSSObject {
  constructor (mark, positions) {
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

  map (x, y) {
    this.cels.forEach(cel => cel.map())
  }

  unmap (x, y) {
    this.cels.forEach(cel => cel.unmap())
  }
}

export { KiSSObject }
