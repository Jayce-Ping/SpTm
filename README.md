# Space-Time (SpTm)

# A Differential Geometry Calculations Package for Wolfram Mathematica

- [中文](#中文)

- [English](#English)

## 中文

*谨以此纪念我的微分几何与广义相对论的入门导师——梁灿彬教授。*

- **Space-Time**是一个关于微分几何计算的**Mathematica程序包**，且与广义相对论关联性较强。
- 具体使用方法和例子见**Document.nb**。
- 这里简单说明如何安装和使用：
    - 任意打开一个Mathematica笔记本，执行`URLDownload["https://raw.githubusercontent.com/YunfatChow/SpTm/main/SpTm.wl", FileNameJoin[{$UserBaseDirectory, "Applications", "SpTm.wl"}]]`。在这之后，在任何笔记本中导入SpTm只需要执行`` <<SpTm` ``或 ``Needs["SpTm`"]``即可。
    - 将``SpTm.wl``文件下载至当前目录，在当前目录下新建笔记本，在笔记本中执行``Get[FileNameJoin[{NotebookDirectory[], "SpTm.wl"}]]``即可。

> 由Brock University的Barak Shoshany所开发的[OGRe](https://github.com/bshoshany/OGRe)程序包也是一个很优秀的微分几何与广义相对论的计算程序包。

## English

*In honor of my introductory tutor in Differential Geometry and General Relativity, Prof. Canbin Liang.*

- **Space-Time** is a Mathematica package for **Differential Geometry** calculations，which is also relevant to **General Relativity** specially。
- For details and examples of use, see **Document.nb**。
- Here is a brief explanation of how to install and use：
    - Open any Mathematica notebook，run ``URLDownload["https://raw.githubusercontent.com/YunfatChow/SpTm/main/SpTm.wl", FileNameJoin[{$UserBaseDirectory, "Applications", "SpTm.wl"}]]``。After this，to import SpTm in any notebook just execute `` <<SpTm` `` or  ``Needs["SpTm`"]``。
    - Download ``SpTm.wl`` to the current directory，create a new Notebook，and run ``Get[FileNameJoin[{NotebookDirectory[], "SpTm.wl"}]]`` in the Notebook。

> The [OGRe](https://github.com/bshoshany/OGRe), developed by Barak Shoshany of Brock University, is also an excellent package for calculating differential geometry and general relativity.
