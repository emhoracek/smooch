
class DragAndDrop {
  constructor () {
    this.dragHandler = false
    this.screen = document.getElementById('screen')
    this.canvas = document.getElementById('ghost')
    this.ctxt = this.canvas.getContext('2d')
  }

  initialize (doll) {
    document.addEventListener('mousedown', (e) => this.onMouseDown(e, doll))
    document.addEventListener('mouseup', () => {
      if (this.dragHandler) {
        document.removeEventListener('mousemove', this.dragHandler)
        this.dragHandler = false
      }
    })
  }

  onMouseDown (e, doll) {
    const pos = getMousePos(this.screen, e)
    const obj = doll.getSelectedObject(pos)
    if (obj) {
      const dragStart = toDragStart(doll, obj, pos)
      this.dragHandler = (e) => onMouseMove(e, this.screen, obj, dragStart, doll)
      document.addEventListener('mousemove', this.dragHandler)
      e.preventDefault()
    }
  }
}

function toDragStart (doll, obj, pos) {
  const objectPosition = doll.getObjectPosition(obj)
  return {
    x: objectPosition.x - pos.x,
    y: objectPosition.y - pos.y
  }
}

function onMouseMove (e, canvas, obj, dragStart, doll) {
  const currentMousePosition = getMousePos(canvas, e)
  const x = dragStart.x + currentMousePosition.x
  const y = dragStart.y + currentMousePosition.y

  doll.moveObject(obj, x, y)

  e.preventDefault()
}

// from http://www.html5canvastutorials.com/advanced/html5-canvas-mouse-coordinates/
const getMousePos = function (canvas, evt) {
  const rect = canvas.getBoundingClientRect()
  return {
    x: evt.clientX - rect.left,
    y: evt.clientY - rect.top
  }
}

export { DragAndDrop }
