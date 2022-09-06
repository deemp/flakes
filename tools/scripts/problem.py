from dataclasses import dataclass
import yaml
import os

# write a template into several files
contest = "Contest"

template = lambda name: \
f'''\
module Contest.{name}(main, readInts, readString, toDoubles, toDouble, ceil, floor) where
import Data.Functor ((<&>))
import GHC.Float.RealFracMethods (ceilingDoubleInt, floorDoubleInt)
import Prelude hiding (floor)

readInts :: IO [Int]
readInts = getLine <&> words <&> fmap read

readString :: IO String
readString = getLine

toDoubles :: (Functor f, Integral a) => f a -> f Double
toDoubles xs = (\\x -> fromIntegral x :: Double) <$> xs

toDouble :: (Integral a) => a -> Double
toDouble = fromIntegral

ceil :: Double -> Int
ceil = ceilingDoubleInt

floor :: Double -> Int
floor = floorDoubleInt

main :: IO ()
main = print "hello, world!"'''


# add or remove files

# https://github.com/yaml/pyyaml/issues/127#issuecomment-525800484
class MyDumper(yaml.SafeDumper):
    # HACK: insert blank lines between top-level objects
    # inspired by https://stackoverflow.com/a/44284819/3786245
    def write_line_break(self, data=None):
        super().write_line_break(data)

        if len(self.indents) == 1:
            super().write_line_break()

@dataclass
class Add: repr = "add"

@dataclass
class Rm: repr = "rm"

@dataclass
class Unsupported: pass

@dataclass
class Unit: pass

@dataclass
class Failure: pass

Command = Add | Rm | Unsupported

unit = Unit()

fail = Failure()

def handle_command(mode: str) -> Command | Failure:
  def command_():
    match mode:
      case Add.repr: return Add()
      case Rm.repr: return Rm()
      case _ : return None
  
  command = command_()
  
  match command:
    case None:
      print(f'Unsupported command "{mode}"!')
      return Unsupported()
    case Add(): pass
    case Rm(): pass
  
  try:
    os.makedirs(name = contest, exist_ok=True)
  except Exception as e:
    print(e)
    print(f"Can't create a directory {contest}")
    return Unsupported()

  return command
  
def write_template(name: str, command: Command) -> Unit | None:
  src = f'./{contest}/{name}.hs'
  
  match command:
    case Add():      
      with open(src, 'w') as f:
        f.write(template(name))
    case _:
      try:
        os.remove(src)
      except Exception as e:
        print(e)
        print(f"You should first add this problem via `problem add {name}`")
  return unit

def handle_package_yaml(name: str, src: str, command: Command) -> Unit:
# handle package.yaml
  package_yaml = "package.yaml"
  
  t: dict = {}
  with open(f'{package_yaml}', 'r') as f:
      try:
        t = yaml.safe_load(f)
      except yaml.YAMLError as e:
        print(e)
        return unit
      except FileNotFoundError as e:
        print(e)
        return unit
      
  executables = "executables"
  if not t[executables]: 
    t[executables] = {}
    
  name_exe = f"{name}-exe"
  match command:
    case Add():      
      t[executables][name_exe] = {"main": f"{src}"}
    case _:
      t[executables].pop(name_exe)

  with open(f'{package_yaml}', 'w') as f:
      try:
        yaml.dump(data = t, stream = f, Dumper=MyDumper,sort_keys=False)
      except yaml.YAMLError as exc:
        print(exc)
        return unit
      
  return unit


def problem(mode: str, name: str) -> Unit | Failure:
  
  command: Command | Failure = handle_command(mode=mode)
  
  match command:
    case Unsupported() | Failure(): return fail
  
  match write_template(name, command):
    case Failure(): return fail
  
  src = f'./{contest}/{name}.hs'
  
  match handle_package_yaml(name, src, command):
    case Failure(): return fail
  
  # print(json.dumps(t, indent=4))
  match command:
    case Add():
      print(f"Added problem {name} as {src}")
    case Rm():
      print(f"Removed problem {name} ({src})")
  
  return unit