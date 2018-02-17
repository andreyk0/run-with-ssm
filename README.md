# run-with-ssm
Runs given command in a modified shell environment, populated from AWS [SSM](https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html) parameters.
In particular this allows injection of secrets via environmental variables.

```bash
Runs given command in a modified shell environment, populated from SSM
parameters.

Usage: run-with-ssm [-v|--verbose] [-V|--version] [-p|--prefix /ssm/prefix]
                    ENV_FOO=ssm/path/foo ENV_BAR=ssm/path/bar cmd -opt1 ... arg1
  Runs given command in a modified shell environment, populated from SSM
  parameters.Source: https://github.com/andreyk0/run-with-ssm

Available options:
  -h,--help                Show this help text
  -v,--verbose             Be verbose.
  -V,--version             Print version and exit.
  -p,--prefix /ssm/prefix  prefix applied to all SSM parameters

See https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html for SSM docs.

  E.g. $ run-with-ssm --prefix /svc-foo/ -- BAR=bar BAZ=baz someprog -arg1 -arg2 ...
```
