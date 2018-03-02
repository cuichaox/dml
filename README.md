
# English
## Introduction
Diagram Make Language for common lisp.  Base on [tkych/donuts](https://github.com/tkych/donuts)

## Examples

#### Code 1
```lisp
($ (:outfile "dml.png")
   (& (:rankdir :LR)
     (-dep-- "use"
             (comp "DML")
             (comp "donuts")
             (comp "Graphviz"))))
```
#### Result 1
![dml.png](https://raw.githubusercontent.com/cuichaox/dml/master/demo/dml.png) </td>


## TODO
To suport Sequnce language

# Chinese 中文
# 介绍
基于[tkych/donuts](https://github.com/tkych/donuts), 使用Common Lisp 快速生成UML图，
donuts是基于[Graphviz](http://www.graphviz.org/),Graphviz有很好的自动布局能力

# 准备做
支持序列图

