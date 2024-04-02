# plt

A command line tool for generating graphs from CSV data.

## example
  
```zsh
plt "[col1, col2, col3], date { bar 10px [solid red, dashed green, dotted blue] }" < data.csv > graph.png
```

## fun

```zsh
pip install imgcat
plt "[col1, col2, col3], date { bar 10px [solid red, dashed green, dotted blue] }" < data.csv | imgcat
```

[imgcat](https://pypi.org/project/imgcat/)