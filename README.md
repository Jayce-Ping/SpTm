# Space-Time (SpTm)

# A Wolfram Mathematica Package for Differential Geometry and General Relativity Calculations.

- [中文](#中文)

- [English](#English)

## 中文

*谨以此纪念我的微分几何与广义相对论的入门导师——梁灿彬教授。*

### 简介

- **Space-Time**是一个关于**微分几何**和**广义相对论**计算的**Mathematica程序包**。

- 你可以使用一种符合书写习惯的方式输入带有抽象指标的张量表达式，并对其进行一些化简和计算。

- 在选取坐标系并设置了张量的分量后，你可以计算得到表达式的分量。

- 你可以计算一些常用张量的分量，如：黎曼张量、里奇张量、爱因斯坦张量等。

- 具体使用方法和例子见[**Document_CN.nb**](https://github.com/Jayce-Ping/SpTm/blob/main/Document_CN.nb)。


### 安装
这里介绍两种安装和使用的方法：

- 任意打开一个Mathematica笔记本，执行
  
    ```mathematica
    URLDownload["https://raw.githubusercontent.com/Jayce-Ping/SpTm/main/SpTm.wl", FileNameJoin[{$UserBaseDirectory, "Applications", "SpTm.wl"}]]
    ```
    
    在这之后，在任何笔记本中导入SpTm只需要执行`` <<SpTm` ``或 ``Needs["SpTm`"]``即可。
    
- 将``SpTm.wl``文件下载至当前目录，在当前目录下新建笔记本，在笔记本中执行
  
    ```mathematica
    Get[FileNameJoin[{NotebookDirectory[], "SpTm.wl"}]]
    ```
    
    即可

> 另外，由Brock University的Barak Shoshany所开发的[OGRe](https://github.com/bshoshany/OGRe)程序包也是一个很优秀的微分几何与广义相对论的计算程序包。

## English

*In honor of my introductory tutor in Differential Geometry and General Relativity, Prof. Canbin Liang.*

### Introduction

- **Space-Time** is a **Mathematica package** for **Differential Geometry** and **General Relativity** calculations.

- You can input tensor expressions with abstract indices in a customary way, and do some simplifications and calculations.

- Given a coordinate system and the components of tensors set, you can calculate the components of the expression.


- You can calculate the components of some common tensors, such as Riemann tensor, Ricc tensor and Einstein tensor, etc.

- For details and examples of use, see [**Document_EN.nb**](https://github.com/Jayce-Ping/SpTm/blob/main/Document_EN.nb).

### Installation

Here are two ways to install and use:

  - Open any Mathematica notebook, run
    
    ```mathematica
    URLDownload["https://raw.githubusercontent.com/Jayce-Ping/SpTm/main/SpTm.wl", FileNameJoin[{$UserBaseDirectory, "Applications", "SpTm.wl"}]]
    ```
    
    After this, to import SpTm in any notebook just execute `` <<SpTm` `` or  ``Needs["SpTm`"]``.
    
- Download ``SpTm.wl`` to the current directory, create a new Notebook, and run
  
    ```mathematica
    Get[FileNameJoin[{NotebookDirectory[], "SpTm.wl"}]]
    ```
    
    in the notebook.

> In addition, the [OGRe](https://github.com/bshoshany/OGRe), developed by Barak Shoshany of Brock University, is also an excellent package for calculating differential geometry and general relativity.
