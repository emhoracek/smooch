class KiSSObject {
  constructor (mark, color, positions) {
    this.currentSet = 0
    this.color = color
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

  setPosition (x, y) {
    this.positions[this.currentSet].x = x
    this.positions[this.currentSet].y = y
  }
}

export { KiSSObject }
