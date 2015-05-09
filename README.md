```js
l.parse('a= emp"a \\\"value"')                                                                       │ 27     let s = "a=foo b=10ms c=cat E=\"123\" d foo= emp="
// { a: null, 'empa value': true }
```

```python
l.parse_line('a= emp="a \\\"value"')
# {'a': True, 'emp': 'a "value'}
```
