function makeAction (a, doll) {
  const actionMap = {
    timer: mkTimer,
    map: mkMap,
    unmap: mkUnmap
    /*
    almap
    changecol
    changeset
    move
    nop
    shell
    sound
    transparent
    unmap
    viewport
    windowsize
    */
  }
  const defaultAction = () => { console.log('Unknown action', a.action) }

  const mkAction = actionMap[a.action] || defaultAction
  return mkAction(a.args, doll)
}

function mkTimer (args, doll) {
  const alarmId = args[0]
  const duration = args[1]
  return () => setTimeout(() => doll.dispatchEvent(mkAlarm(alarmId)), duration)
}

function mkMap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.map.bind(objOrCel)
  } else {
    return () => console.log('Unable to find cel', args[0])
  }
}

function mkUnmap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.unmap.bind(objOrCel)
  } else {
    return () => console.log('Unable to find object or cel', args[0])
  }
}

function mkAlarm (n) {
  return new CustomEvent('alarm' + n)
}

function objOrCelArg (arg, doll) {
  if (typeof arg === 'number') {
    const obj = doll.getObject(arg)
    if (obj) {
      return obj
    } else {
      console.log('Unable to find object with mark', arg)
    }
  }
  if (arg.endsWith('.cel')) {
    const cel = doll.getCel(arg)
    if (cel) {
      return cel
    } else {
      console.log('Unable to find cel with filename', arg)
    }
  }
  console.log('Expected a cel or object reference but got: ', arg)
}

export { makeAction, objOrCelArg }
