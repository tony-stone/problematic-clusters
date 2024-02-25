# Presentation template

A template repository for presentations (with ECHILD logo).

Remember to create an empty `gh-pages` branch:
```
git switch --orphan gh-pages
git commit --allow-empty -m "Initial commit on gh-pages branch"
git push -u upstream gh-pages
git switch main
```

To create a remote (upstream) repo from a local copy:
```
gh repo create --public --source=. --remote=upstream --push
```
