class DragAndDrop {
  constructor (doll) {
    this.dragHandler = false
    this.doll = doll
    this.screen = document.getElementById('screen')
    const canvas = document.getElementById('ghost')
    this.ctxt = canvas.getContext('2d')
  }

  initialize () {
    document.addEventListener('mousedown', (e) => this.onMouseDown(e))
    document.addEventListener('mouseup', () => {
      if (this.dragHandler) {
        document.removeEventListener('mousemove', this.dragHandler)
        this.dragHandler = false
      }
    })
  }

  onMouseDown (e) {
    const pos = this.getMousePos(e)
    const obj = this.doll.getSelectedObject(pos)
    if (obj) {
      const dragStart = this.getDragStart(obj, pos)
      this.dragHandler = (e) => this.onMouseMove(e, obj, dragStart)
      document.addEventListener('mousemove', this.dragHandler)
      e.preventDefault()
    }
  }

  getDragStart (obj, pos) {
    const objectPosition = this.doll.getObjectPosition(obj)
    return {
      x: objectPosition.x - pos.x,
      y: objectPosition.y - pos.y
    }
  }

  onMouseMove (e, obj, dragStart) {
    const currentMousePosition = this.getMousePos(e)
    const x = dragStart.x + currentMousePosition.x
    const y = dragStart.y + currentMousePosition.y

    this.doll.moveObject(obj, x, y)

    e.preventDefault()
  }

  // from http://www.html5canvastutorials.com/advanced/html5-canvas-mouse-coordinates/
  getMousePos (evt) {
    const rect = this.screen.getBoundingClientRect()
    return {
      x: evt.clientX - rect.left,
      y: evt.clientY - rect.top
    }
  }
}

export { DragAndDrop }
