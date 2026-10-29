# dbc — Vector DBC (CAN database) in pure Racket

[English](README.md) · **中文**

[![CI](https://github.com/turlamai/dbc/actions/workflows/ci.yml/badge.svg)](https://github.com/turlamai/dbc/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

`dbc` 读取和写入 **DBC 文件**——由 Vector Informatik 发明、汽车行业几乎所有 CAN 工具都在使用的事实标准 CAN 数据库格式。DBC 文件描述 CAN 总线的符号层：报文与信号名称、位布局、缩放、值表、注释与属性。

全部以纯 Racket 实现：无 FFI，无系统依赖。信号编解码器在 cantools 自己的测试语料上与 Python 的 [cantools](https://github.com/cantools/cantools) 做了逐位对照验证（其示例文件作为测试数据一并发布）。

## 安装

```
raco pkg install dbc
```

## 用法

解析后的文件是一个 `dbc?` 值，持有两个视图：

- **条目流** —— `dbc-entries` 按源顺序保留每个语法元素（`BO_`、`CM_`、`BA_DEF_`、`VAL_` 等），这正是无损序列化得以实现的原因（`serialize-dbc` 往返后 `equal?` 完全相等，包括以 `entry-unparsed` 保留的未知厂商段）；
- **编解码视图** —— 每个 `entry-bo` 包装一个 `message?`，其 `signal?` 携带附加的选项值/注释/浮点类型，随时可用于 `message-decode` / `message-encode`。

```racket
#lang racket/base
(require dbc)

;; A self-contained synthetic Vector DBC; no external .dbc file needed.
(define db (parse-dbc #<<DBC
VERSION "1.0"

BO_ 37 ExampleMessage: 8 Vector__XXX
 SG_ VehicleSpeed : 39|16@1+ (0.01,0) [-100|100] "kph" Vector__XXX
 SG_ Temperature : 0|12@0- (0.01,250) [-40|215] "degC" Vector__XXX
 SG_ DriveMode : 56|8@1+ (1,0) [0|1] "" Vector__XXX

VAL_ 37 DriveMode 0 "parked" 1 "active" ;
DBC
))
(define msg
  (for/first ([e (in-list (dbc-entries db))] #:when (entry-bo? e))
    (entry-bo-message e)))

(message-name msg) ; => 'ExampleMessage
(message-decode msg (make-bytes 8 0) #:decode-choices? #t)
;; DriveMode is "parked" according to VAL_; scaled VehicleSpeed stays numeric.
(define payload
  (message-encode msg (hash 'Temperature 250.47 'VehicleSpeed 0.5)))
(message-decode msg payload)
(equal? (parse-dbc (serialize-dbc db)) db) ; => #t
```

### 位编号

负载的第 `n` 位位于第 `(quotient n 8)` 字节、从 LSB 起第 `(remainder n 8)` 位。

- **Intel**（`@1`，小端）：值的 LSB 位于起始位；位 `start … start+len-1` 按 LSB 在前的方式累积。
- **Motorola**（`@0`，大端）：值的 MSB 位于起始位；其余各位沿锯齿形行走——从一个字节的 LSB 跳到下一字节的 MSB（+15），否则下移一位（-1）——按 MSB 在前的方式累积。这就是 cantools 产生的位放置方式（已对照其探针逐位验证）。

### 多路复用

普通多路复用（`M` 开关、`mN` 变体）与扩展多路复用（`SG_MUL_VAL_` 区间）均可解码和编码。同名的复用变体保留为独立信号；解码结果中只出现当前开关值下活跃的变体。

## API 手册、可运行示例与测试

[完整 Scribble API 文档](dbc/scribblings/dbc.scrbl)说明条目模型、
无损序列化、Intel/Motorola 位布局、信号缩放和枚举表；
手册中的合成 CAN DBC 示例会在文档构建时执行。
[独立 quickstart](examples/quickstart.rkt)检查消息查找、
CAN 帧编解码、未缩放 DriveMode 的 VAL_ 选项值与 DBC 序列化，不依赖 CAN 硬件、
外部文件或专有数据。

安装 Racket 后，在仓库根目录执行：

```sh
raco pkg install --auto --link "$(pwd)"
raco test -p dbc
racket examples/quickstart.rkt
raco setup --check-pkg-deps -l dbc
```

CI 在 Linux、macOS、Windows 上验证这些命令。
`dbc` 是 multi-collection 包的公开 collection；解析 DBC 文件
不等于已经连接或操作实时 CAN 总线。

## 局限

- 属性（`BA_DEF_` / `BA_`）会被解析并往返保留，但其语义不做解释。
- 环境变量（`EV_`）、`SIG_GROUP_` 与 `SGTYPE_` 家族按结构保留（语法不规则之处以不透明条目保存）。
- J1939 PGN 约定不在范围内；扩展 ID 位通过 `message-extended?` 暴露。

## 许可证

[MIT](LICENSE)。测试语料包含来自 [cantools](https://github.com/cantools/cantools) 项目的示例文件（MIT）。
