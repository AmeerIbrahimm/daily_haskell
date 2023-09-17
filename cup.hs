cup f10z message = message f10z

getOz aCup = aCup (\f10z -> f10z)

drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    f10z = getOz aCup
    ozDiff = f10z - ozDrank

isEmptyCup aCup = getOz aCup == 0

-- robot Example

robot (name, attack, hp) message = message (name, attack, hp)

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, h) = h

-- Getters

getName robot = robot name

getAttack robot = robot attack

getHP robot = robot hp

-- setters

setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))

setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\(n, a, _) -> robot (n, a, newHP))

-- print

printRobot aRobot =
  aRobot
    ( \(n, a, h) ->
        n
          ++ " attack: "
          ++ show a
          ++ " hp: "
          ++ show h
    )
