class DragAndDrop {
  constructor (doll) {
    this.dragHandler = false
    this.releaseHandler = false
    this.doll = doll
    this.screen = document.getElementById('screen')
    const canvas = document.getElementById('ghost')
    this.ctxt = canvas.getContext('2d')
  }

  initialize () {
    document.addEventListener('pointerdown', (e) => {
      if (this.releaseHandler) {
        document.removeEventListener('pointerup', this.releaseHandler)
        this.releaseHandler = false
      } 
      this.onMouseDown(e)
    })
  }

  onRelease (e, objAndCel) {
    if (objAndCel) {
      objAndCel.object.dispatchEvent(release)
      objAndCel.cel.dispatchEvent(release)
    }

    if (this.dragHandler) {
      document.removeEventListener('pointermove', this.dragHandler)
      this.dragHandler = false
    }
  }

  onMouseDown (e) {
    const pos = this.getMousePos(e)
    const objAndCel = this.doll.getSelectedObject(pos)
    if (objAndCel) {
      objAndCel.object.dispatchEvent(press)
      objAndCel.cel.dispatchEvent(press)

      this.releaseHandler = (e) => this.onRelease(e, objAndCel)
      document.addEventListener('pointerup', this.releaseHandler)

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

const press = new CustomEvent('press')
const release = new CustomEvent('release')

export { DragAndDrop }
