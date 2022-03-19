// A KiSS Cel represents each instance of an image appearing in the
// doll. It doesn't represent the image file itself. So a single
// image can appear multiple times in multiple objects as different
// KiSSCels
class KiSSCel {
  constructor (obj, cel, set, incLoaded) {
    this.name = cel.name
    this.mark = cel.mark
    this.id = cel.name
    this.fix = cel.fix
    this.position = obj.positions[0]
    this.positions = obj.positions
    this.sets = cel.sets
    this.image = document.getElementById(cel.name + '-' + cel.palette)
    this.ghostImage = undefined
    this.visible = false
    this.mapped = true
    this.alpha = cel.alpha
    this.currentSet = 0

    this.offset = cel.offset

    this.init(set)

    // Let Smooch know when image is loaded
    this.ghostImage.onload = function () {
      incLoaded()
    }

    return this
  }

  init (set) {
    const drawctxt = set.ctxt
    const drawcanvas = set.canvas
    const image = this.image

    // Draw image to ctxt and get image data
    drawctxt.drawImage(image, 0, 0, image.width, image.height)

    const ghostImageData = drawctxt.getImageData(0, 0, image.width, image.height)
    const data = ghostImageData.data

    // Fill ghost image data with obj color
    const color = decimalToRgb(this.mark)
    for (let k = 0; k < data.length; k = k + 4) {
      data[k] = color.red
      data[k + 1] = color.green
      data[k + 2] = color.blue
    }

    // Clear ctxt and draw altered image
    drawctxt.clearRect(0, 0, set.size.x, set.size.y)
    drawctxt.putImageData(ghostImageData, 0, 0)

    // Save altered image as cel's ghost image
    this.ghostImage = new Image()
    this.ghostImage.src = drawcanvas.toDataURL('image/png')

    // Clear ctxt
    drawctxt.clearRect(0, 0, set.size.x, set.size.y)
  }

  update (currentSet) {
    this.currentSet = currentSet
    if (this.sets.indexOf(currentSet) === -1 || !this.mapped) {
      this.visible = false
    } else {
      this.visible = true
    }
    this.position = this.positions[currentSet]
  }

  draw (screen, ghost) {
    if (this.visible === true) {
      if (this.alpha) {
        screen.globalAlpha = (255 - this.alpha) / 255
      }

      screen.drawImage(
        this.image,
        this.position.x + this.offset.x,
        this.position.y + this.offset.y
      )

      screen.globalAlpha = 1

      ghost.drawImage(
        this.ghostImage,
        this.position.x + this.offset.x,
        this.position.y + this.offset.y
      )
    }
  }

  map () {
    this.mapped = true
  }

  unmap () {
    this.mapped = false
  }
}

// thanks to DrBracewell#0252 on The Coding Den Discord
function decimalToRgb (n) {
  return {
    red: (n & 0xff0000) >> 16,
    green: (n & 0x00ff00) >> 8,
    blue: (n & 0x0000ff)
  }
}

export { KiSSCel }
