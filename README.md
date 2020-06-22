
# Bing

用Haskell实现的Bing壁纸系列工具，Haskell的练习项目

- [x] bing-core: 通用库
- [x] bing-data: 封装数据库操作
- [x] bing-bot: 爬虫相关API的封装
- [x] bing-task: 定时去获取当日壁纸信息，下载并保存到数据库中
- [x] bing-rest: 提供RESTfull API
- [ ] bing-cli:  提供一个命令行工具

# Build

```bash
cd bing-haskell
stack build
```

国内网络可能无法下载，需要使用代理或者vpn
