import yaml
import os


# write a template into several files
contest = "contest"

template = (
  'main :: IO ()\n'
  'main = print "hello, world!"'
)

def write_templates():
  os.makedirs(path = contest, exist_ok=True)
  for i in range(ord('A'),ord('Z')+1):
    with open(f'{contest}/{chr(i)}.hs', 'w') as f:
      f.write(template)


# add or remove files

# https://github.com/yaml/pyyaml/issues/127#issuecomment-525800484
class MyDumper(yaml.SafeDumper):
    # HACK: insert blank lines between top-level objects
    # inspired by https://stackoverflow.com/a/44284819/3786245
    def write_line_break(self, data=None):
        super().write_line_break(data)

        if len(self.indents) == 1:
            super().write_line_break()

def problem(mode: str, name: str):
  ADD = "add"
  RM = "rm"
  
  if mode != ADD and mode != RM:
    print(f'Unrecognized command "{mode}"!')
    return
  
  
  try:
    os.makedirs(name = contest, exist_ok=True)
  except Exception as exc:
    print(exc)
    return
  
  src = f'./{contest}/{name}.hs'
  
  if mode == ADD:
    with open(src, 'w') as f:
        f.write(template)
  elif mode == RM:
    try:
      os.remove(src)
    except Exception as exc:
      print(exc)
      print(f"You should first add this problem via `problem add {name}`")
      return
        
  # handle package.yaml
  package_yaml = "package.yaml"
  t = None
  with open(f'{package_yaml}', 'r') as f:
      try:
        t = yaml.safe_load(f)
      except yaml.YAMLError as exc:
        print(exc)
        return
  try:
    executables = "executables"
    if not t[executables]:
      t[executables] = {}
    
    p = t[executables]
    exe = f"{name}-exe"
    if mode == ADD:
      p[exe] = {"main": f"{src}"}
    elif mode == RM:
      p.pop(exe)
  except Exception as exc:
    print(exc)
    return
  
  with open(f'{package_yaml}', 'w') as f:
      try:
        yaml.dump(data = t, stream = f, Dumper=MyDumper,sort_keys=False)
      except yaml.YAMLError as exc:
        print(exc)
  
  # handle hie.yaml
  hie_yaml = "hie.yaml"
  t = None
  with open(f'{hie_yaml}', 'r') as f:
      try:
        t = yaml.safe_load(f)
      except yaml.YAMLError as exc:
        print(exc)
        return
  try:
    cradle = "cradle"
    stack = "stack"
    if not t[cradle] or t[cradle] and not t[cradle][stack]:
      t[cradle] = {stack : {}}
      
    exe = f"{name}-exe"
    p = t[cradle][stack]
    component = f"acpoj:exe:{name}-exe"
    s = list(filter(lambda x: not (x["path"] == src), p))
    if mode == ADD:
      p = s + [{"path" : src, "component": component}]
    elif mode == RM:
      p = s
    t[cradle][stack] = p
  except Exception as exc:
    print(exc)
    return
  
  with open(f'{hie_yaml}', 'w') as f:
      try:
        yaml.dump(data = t, stream = f, Dumper=MyDumper,sort_keys=False)
      except yaml.YAMLError as exc:
        print(exc)
    
  # print(json.dumps(t, indent=4))
          
  if mode == ADD:
    print(f"Added problem {name} as {src}")
    
  elif mode == RM:
    print(f"Removed problem {name} ({src})")


# import yaml
# import os
# from dataclasses import dataclass

# # write a template into several files
# contest = "contest"

# template = (
#   'main :: IO ()\n'
#   'main = print "hello, world!"'
# )

# def write_templates():
#   os.makedirs(path = contest, exist_ok=True)
#   for i in range(ord('A'),ord('Z')+1):
#     with open(f'{contest}/{chr(i)}.hs', 'w') as f:
#       f.write(template)


# # add or remove files

# # https://github.com/yaml/pyyaml/issues/127#issuecomment-525800484
# class MyDumper(yaml.SafeDumper):
#     # HACK: insert blank lines between top-level objects
#     # inspired by https://stackoverflow.com/a/44284819/3786245
#     def write_line_break(self, data=None):
#         super().write_line_break(data)

#         if len(self.indents) == 1:
#             super().write_line_break()

# ADD = "add"
# RM = "rm"

# @dataclass
# class Args:
#   name: str
#   src: str
#   package_yaml: str
#   execuatables: str
#   exe: str

# def add_problem(a: Args):
#   with open(a.src, 'w') as f:
#     f.write(template)
    
#   t = None
#   with open(f'{a.package_yaml}', 'r') as f:
#       try:
#         t = yaml.safe_load(f)
#       except yaml.YAMLError as exc:
#         print(exc)
#         return
#       except Exception as exc:
#         print(f"Can't open {a.package_yaml}")
#         return

#   if not t[a.executables]:
#     t[a.executables] = {}
#   t[a.executables][a.exe] = {"main": f"{a.src}"}
  
#   with open(f'{a.package_yaml}', 'w') as f:
#       try:
#         yaml.dump(data = t, stream = f, Dumper=MyDumper,sort_keys=False)
#       except yaml.YAMLError as exc:
#         print(exc)
  

# def rm_problem(a: Args):
#   os.remove(a.src)
#   t = None
#   with open(f'{package_yaml}', 'r') as f:
#       try:
#         t = yaml.safe_load(f)
#       except yaml.YAMLError as exc:
#         print(exc)
#         return
#   try:
#     executables = "executables"
#     if not t[executables]:
#       t[executables] = {}
    
#     p = t[executables]
#     exe = f"{name}-exe"
#     if mode == ADD:
#       p[exe] = {"main": f"{src}"}
#     elif mode == RM:
#       p.pop(exe)
#   except Exception as exc:
#     print(exc)
#     return
  
#   with open(f'{package_yaml}', 'w') as f:
#       try:
#         yaml.dump(data = t, stream = f, Dumper=MyDumper,sort_keys=False)
#       except yaml.YAMLError as exc:
#         print(exc)
  

# def problem(mode: str, name: str):
  
  # if mode != ADD and mode != RM:
  #   print(f'Unrecognized command "{mode}"!')
  #   return
  
  # try:
  #   os.makedirs(name = contest, exist_ok=True)
  # except Exception as exc:
  #   print(exc)
  #   print(f"Can't create ./{contest}")
  #   return
  
  # src = f'./{contest}/{name}.hs'
  # package_yaml = "package.yaml"
  # executables = "executables"
  # exe = f"{name}-exe"
  
  # args = Args(name, src, package_yaml, executables,exe)
  # if mode == ADD:
  #   add_problem(args)
  # else:
  #   rm_problem(args)
        
  # # handle package.yaml
  # package_yaml = "package.yaml"
  # t = None
  # with open(f'{package_yaml}', 'r') as f:
  #     try:
  #       t = yaml.safe_load(f)
  #     except yaml.YAMLError as exc:
  #       print(exc)
  #       return
  # try:
  #   executables = "executables"
  #   if not t[executables]:
  #     t[executables] = {}
    
  #   p = t[executables]
  #   exe = f"{name}-exe"
  #   if mode == ADD:
  #     p[exe] = {"main": f"{src}"}
  #   elif mode == RM:
  #     p.pop(exe)
  # except Exception as exc:
  #   print(exc)
  #   return
  
  # with open(f'{package_yaml}', 'w') as f:
  #     try:
  #       yaml.dump(data = t, stream = f, Dumper=MyDumper,sort_keys=False)
  #     except yaml.YAMLError as exc:
  #       print(exc)
  
  # # handle hie.yaml
  # hie_yaml = "hie.yaml"
  # t = None
  # with open(f'{hie_yaml}', 'r') as f:
  #     try:
  #       t = yaml.safe_load(f)
  #     except yaml.YAMLError as exc:
  #       print(exc)
  #       return
  # try:
  #   cradle = "cradle"
  #   stack = "stack"
  #   if not t[cradle] or t[cradle] and not t[cradle][stack]:
  #     t[cradle] = {stack : {}}
      
  #   exe = f"{name}-exe"
  #   p = t[cradle][stack]
  #   component = f"acpoj:exe:{name}-exe"
  #   s = list(filter(lambda x: not (x["path"] == src), p))
  #   if mode == ADD:
  #     p = s + [{"path" : src, "component": component}]
  #   elif mode == RM:
  #     p = s
  #   t[cradle][stack] = p
  # except Exception as exc:
  #   print(exc)
  #   return
  
  # with open(f'{hie_yaml}', 'w') as f:
  #     try:
  #       yaml.dump(data = t, stream = f, Dumper=MyDumper,sort_keys=False)
  #     except yaml.YAMLError as exc:
  #       print(exc)
    
  # # print(json.dumps(t, indent=4))
          
  # if mode == ADD:
  #   print(f"Added problem {name} as {src}")
    
  # elif mode == RM:
  #   print(f"Removed problem {name} ({src})")