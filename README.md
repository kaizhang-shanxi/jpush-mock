# jpush mock

JPush 的 mock 服务器，目前只 mock 了一部分功能，用于内部集成测试。

## 构建镜像

```
cd jpush-mock
docker-compose build
```

## API 文档

```
cd jpush-mock
docker-compose up -d  # 运行容器
docker-compose port jpush-mock 1234  # 记下这个 hostPort
curl http://127.0.0.1:${hostPort}/  # ${hostPort} 为上一行命令的输出
```
