# ghtd

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/mlopes/ghtd/blob/master/LICENSE)

## Usage

```bash
$ ghtd add "Do the dishes" --project Home --contexts=kitchen,sink,home,offline

1. Do the dishes [ ]
Project: Home
Contexts: kitchen, sink, home, offline
(df5fcade2adb49ceb3902f123dabc764)


$ ghtd add "Send letter" --contexts=offline

1. Do the dishes [ ]
Project: Home
Contexts: kitchen, sink, home, offline
(df5fcade2adb49ceb3902f123dabc764)

2. Send letter [ ]
Project: Inbox
Contexts: offline
(c933c8c0e55041f4966d5f326e853d28)

$ ghtd complete df5fcade2adb49ceb3902f123dabc764

1. Do the dishes [✓]
Project: Home
Contexts: kitchen, sink, home, offline
(df5fcade2adb49ceb3902f123dabc764)

$ ghtd cancel c933c8c0e55041f4966d5f326e853d28

1. Sned letter [X]
Project: Inbox
Contexts: offline
(c933c8c0e55041f4966d5f326e853d28)
```

More features to come
