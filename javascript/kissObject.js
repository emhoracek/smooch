class KiSSObject {
  constructor (obj) {
    this.currentSet = 0
    this.positions = obj.positions
    this.position = obj.positions[this.currentSet]
    this.cels = obj.cels

    return this
  }

  update (currentSet) {
    for (let i = 0; i < this.cels.length; i++) {
      this.cels[i].currentSet = currentSet
      this.cels[i].position = this.positions[currentSet]
      this.cels[i].update(currentSet)
    }
  }

  setPosition (set, x, y) {
    this.positions[set].x = x
    this.positions[set].y = y
  }
}

export { KiSSObject }
