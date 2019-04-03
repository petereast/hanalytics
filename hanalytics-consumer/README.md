# hanalytics-consumer

The purpose of this program is to read analytics events from redis,
build aggregated objects out of them and push them into another queue
for another service to consume.
```
+-----------------+        +------------------+        +----------------------------+
|                 |        |                  |        |                            |
| Producer/agent  +------->+ Consumer (this)  +------->+ Viewer/2nd Level Analysis  |
|                 |        |                  |        |                            |
+-----------------+        +------------------+        +----------------------------+
```
