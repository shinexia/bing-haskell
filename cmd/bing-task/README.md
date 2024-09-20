# bing-task

爬虫进程


## 编译

```bash
make
```

## 执行

```bash
./dist/bing-task -c test.conf -o .tmp
```

## cron

run `crontab -e`

```
0 2 * * * /path/to/bing-task -c /path/to/bing-task.conf >> /home/xxx/.logs/bing-task.log
```
