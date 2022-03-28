import { KiSSDoll } from './kissDoll.mjs'
import { DragAndDrop } from './dragAndDrop.mjs'

window.addEventListener('load', function () {
  /* globals kissJson, globalKiss */
  let doll = false
  let loaded = 0
  const totalCels = kissJson.cels.length

  function checkLoaded () {
    console.log(`loading ${loaded} of ${totalCels}`)
    if (loaded < totalCels) {
      window.setTimeout(checkLoaded, 5)
    } else {
      doll.draw()
    }
  }

  window.setTimeout(checkLoaded, 5)

  doll = new KiSSDoll(kissJson, () => { loaded += 1 })
  const dragger = new DragAndDrop(doll)
  dragger.initialize()

  // in testing and development, it's convenient to have access to
  // to the kiss set object
  if (typeof globalKiss !== 'undefined') {
    // eslint-disable-next-line no-global-assign
    globalKiss = doll
  }
})
