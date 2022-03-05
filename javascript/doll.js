import { KiSSDoll } from './kissDoll'
import { DragAndDrop } from './dragAndDrop'

window.addEventListener('load', function () {
  /* globals kissJson, globalKiss */
  let loaded = 0
  const totalCels = kissJson.cels.length
  const doll = new KiSSDoll(kissJson, () => { loaded += 1 })
  const dragger = new DragAndDrop(doll)
  dragger.initialize()

  function checkLoaded () {
    if (loaded < totalCels) {
      console.log(`loading ${loaded} of ${totalCels}`)
      window.setTimeout(checkLoaded, 500)
    } else {
      doll.draw()
    }
  }

  // in testing and development, it's convenient to have access to
  // to the kiss set object
  if (globalKiss) {
    // eslint-disable-next-line no-global-assign
    globalKiss = doll
  }

  window.setTimeout(checkLoaded, 500)
})
