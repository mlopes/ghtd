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

1. Send letter [X]
Project: Inbox
Contexts: offline
(c933c8c0e55041f4966d5f326e853d28)

$ ghtd project

Home
Inbox

$ ghtd context

kitchen
sink
home
offline
```

More features to come

## Plan

Going forward the plan is that there will be something like an application
*scope*. The user sets the application scope, and calling
ghtd will act on that scope. By default it acts on project Inbox.

Here's a description of the planned functionality:

- `ghtd project` - lists projects
- `ghtd context` - lists contexts
- `ghtd project <project name>` - checks out a project into scope
- `ghtd context <context name>` - checks out a context into scope

Alternative to the two above might be `ghtd checkout <name>` which could try to figure by itself if it should checkout a project or a context. Flags could be provided to disambiguate, and by default it would fall back to project. Calling it without any parameters would checkout Inbox. Alternatively, the keyword could be `scope` which is language consistent for the user.

The application will act on the checked out project/ context

- `ghtd` - Will list actions in the currently checked out project/context
- `ghtd complete <action index>` - Marks the action in index position as completed
- `ghtd cancel <action index>` - Marks the action in index position as cancelled
- `ghtd mv <action index> <new index>` - Reorder action from index to new index
- `ghtd add <action name> [-p/--project <project name>] [-c/--contexts <context names>` - Add action. If project and context are ommited, they'll default to the current one. If no project is checked out, it defaults to Inbox, if no context is checked out it defaults to empty
- `ghtd archive <project name>` - Archives project, marks all incomplete actions as cancelled
- `ghtd archive` - Archives all completed/cancelled actions in currently checked out project/context
- `ghtd archive -a <action index>` - Archives action, marks it as cancelled if it's not completed
