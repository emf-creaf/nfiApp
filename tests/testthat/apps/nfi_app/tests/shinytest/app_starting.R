app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("app_starting")

app$snapshot()
