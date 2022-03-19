function makeAction (a, doll) {
  const actionMap = {
    timer: mkTimer,
    map: mkMap,
    unmap: mkUnmap
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
  const cel = doll.getCel(args[0])
  if (cel) {
    return cel.map.bind(cel)
  } else {
    return () => console.log('Unable to find cel', args[0])
  }
}

function mkUnmap (args, doll) {
  const cel = doll.getCel(args[0])
  if (cel) {
    return cel.unmap.bind(cel)
  } else {
    return () => console.log('Unable to find cel', args[0])
  }
}

function mkAlarm (n) {
  return new CustomEvent('alarm' + n)
}

export { makeAction }
