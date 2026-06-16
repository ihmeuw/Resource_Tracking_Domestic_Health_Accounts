from __future__ import annotations
import yaml
from therepy import Here
from pathlib import Path

here = Here("*.Rproj")

_PARAMS_YML_FILE = here.here("data", "meta", "params.yml")
_PATHS_YML_FILE = here.here("data", "meta", "paths.yml")


class Params:
    def __init__(self, yaml_path) -> None:
        with open(yaml_path, "r") as f:
            tmp = yaml.safe_load(f)
        self.params = {str(k).lower(): v for k, v in tmp.items()}

    def __call__(self,
                 param: str,
                 sub_key: str|None=None) -> str|dict:
        if sub_key is None:
            return self.params[param]
        return self.params[param][sub_key]



class Paths:
    def __init__(self, yaml_path) -> None:
        with open(yaml_path, "r") as f:
            self.paths = yaml.safe_load(f)
    def __call__(self,
                 path_type: str,
                 name: str|None=None, *append):
        if name is None:
            return self.paths[path_type]
        return Path(self.paths[path_type][name], "/".join(append))



class ProjectStruct:
    def __init__(self) -> None:
        self.params = Params(_PARAMS_YML_FILE)
        self.paths  = Paths(_PATHS_YML_FILE)


Project = ProjectStruct()