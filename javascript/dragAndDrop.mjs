import { press } from './fkissEvent.mjs'

class DragAndDrop {
  constructor (doll) {
    this.dragHandler = false
    this.doll = doll
    this.screen = document.getElementById('screen')
    const canvas = document.getElementById('ghost')
    this.ctxt = canvas.getContext('2d')
  }

  initialize () {
    document.addEventListener('pointerdown', (e) => this.onMouseDown(e))
    document.addEventListener('pointerup', () => {
      if (this.dragHandler) {
        document.removeEventListener('pointermove', this.dragHandler)
        this.dragHandler = false
      }
    })
  }

  onMouseDown (e) {
    const pos = this.getMousePos(e)
    const objAndCel = this.doll.getSelectedObject(pos)
    if (objAndCel) {
      objAndCel.object.dispatchEvent(press)
      objAndCel.cel.dispatchEvent(press)
      if (!objAndCel.object.fixed) {
        const dragStart = this.getDragStart(objAndCel.object, pos)
        this.dragHandler = (e) => this.onMouseMove(e, objAndCel.object, dragStart)
        document.addEventListener('pointermove', this.dragHandler)
        e.preventDefault()
      }
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
