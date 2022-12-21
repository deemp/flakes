# Database

Here, I'm going to try building a `Haskell` app that accesses a database.
The `PostgreSQL` database is provided by `Helm`.

## Quick start

```sh
nix develop
write-settings-json
codium .
# open a terminal there
nix develop
minikube start
cd postgresql
helm install postgres .
# or
helm upgrade postgres .
# get a password and access a database as proposed
```

## References

- Expose ports - [src](https://docs.bitnami.com/kubernetes/infrastructure/postgresql/get-started/expose-service/)
  - Use `NodePort`
- Haskell template - [src](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme)
