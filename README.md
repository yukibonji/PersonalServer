Reproduce "failed test succeeds when replay attempted"

1) use testexpectofscheck branch
2) run PersonalServer/tests/PersonalServer.Tests/PersonalServerTests

(this will fail, probably not on the first test)

3) take the FsCheck seed of the failing test and paste into let declaration of  configReplay in PersonalServer/tests/PersonalServer.Tests/DomainTypes.fs

4) rebuild solution in debug

5) re-run PersonalServerTests and notice Expecto reports success


@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

3 (possibly related) problems with Expecto FsCheck integration

See the "testexpectofscheck" branch of https://github.com/jackfoxy/personalServer for demonstration code.

The problems have only been tested/verified in debug compile, not release. See the script file \tests\PersonalServer.Tests\Scripts\ArbRegister.fsx to demo the problems in native FsCheck. Note that all reported problems in Expecto/FsCheck can be made to work in ArbRegister.fsx.

tests/PersonalServer.Tests contains the Expecto project.

Demo test list is in DomainTypes.fs.

Problem 1) custom label does not print to console on test failure

Problem 2) failed test succeeds when replay attempted
take reported failure seed and re-run test with configReplay

Problem 3) Arb.register does not work in Expecto
test case returns message, even though Arb.register present:

The type Jackfoxy.PersonalServer.FullName is not handled automatically by FsCheck. Consider using another type or writing and registering a generator for it.