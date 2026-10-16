# AGENTS.md

Vector DBC（CAN 数据库）的纯 Racket 解析与模型（MIT，已上架 Racket catalog）。

## 结构

Multi-collection 包：collection 即 `dbc/` 子目录，scribblings 声明在 `dbc/info.rkt`（collection 级）——改包结构勿破坏这条，否则 catalog 会标 needs-documentation。

## 常用命令

```bash
raco pkg install --auto --link "$(pwd)"
raco test -p dbc
```

## 验证与发版

- CI（`ci.yml`）：即上面两条命令，三平台跑。
- 发版 = tag `v<版本>` + CHANGELOG 同步；catalog 侧为手动提交（pkgd）。
