# Space-Time (SpTm)

# A Differential Geometry Calculations Package for Wolfram Mathematica

- [中文](#中文)

- [English](#English)

## 中文

*谨以此纪念我的微分几何与广义相对论的入门导师——梁灿彬教授。*

- **Space-Time**是一个关于**微分几何**计算的**Mathematica程序包**，且与广义相对论关联性较强。

- 具体使用方法和例子见**Document_CN.nb**。

- 这里介绍两种安装和使用的方法：

    （1）任意打开一个Mathematica笔记本，执行

    ```mathematica
	URLDownload["https://raw.githubusercontent.com/YunfatChow/SpTm/main/SpTm.wl", FileNameJoin[{$UserBaseDirectory, "Applications", "SpTm.wl"}]]
    ```

    在这之后，在任何笔记本中导入SpTm只需要执行`` <<SpTm` ``或 ``Needs["SpTm`"]``即可。

    （2）将``SpTm.wl``文件下载至当前目录，在当前目录下新建笔记本，在笔记本中执行
    
    ```mathematica
	Get[FileNameJoin[{NotebookDirectory[], "SpTm.wl"}]]
    ```

	即可

> 另外，由Brock University的Barak Shoshany所开发的[OGRe](https://github.com/bshoshany/OGRe)程序包也是一个很优秀的微分几何与广义相对论的计算程序包。

## English

*In honor of my introductory tutor in Differential Geometry and General Relativity, Prof. Canbin Liang.*

- **Space-Time** is a **Mathematica package** for **Differential Geometry** calculations, which is also relevant to **General Relativity** specially.

- For details and examples of use, see **Document_EN.nb**.

- Here are two ways to install and use:

    (1) Open any Mathematica notebook, run

    ```mathematica
    URLDownload["https://raw.githubusercontent.com/YunfatChow/SpTm/main/SpTm.wl", FileNameJoin[{$UserBaseDirectory, "Applications", "SpTm.wl"}]]
    ```

    After this, to import SpTm in any notebook just execute `` <<SpTm` `` or  ``Needs["SpTm`"]``.

    (2) Download ``SpTm.wl`` to the current directory, create a new Notebook, and run

    ```mathematica
    Get[FileNameJoin[{NotebookDirectory[], "SpTm.wl"}]]
    ```

     in the notebook.

> In addition, the [OGRe](https://github.com/bshoshany/OGRe), developed by Barak Shoshany of Brock University, is also an excellent package for calculating differential geometry and general relativity.
