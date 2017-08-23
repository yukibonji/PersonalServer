Visual Studio test discovery results in message: 

'vstest.discoveryengine.x86.exe' WCF communication channel unavailable. Active request will be aborted.

(See output window, Tests)

But tests have no problem running in the console app.

Cause: appears to be problem with passing types in the FsCheckConfig.arbitrary field

Reproduction steps:

File: PersonalServer\tests\PersonalServer.Tests\DomainTypes.fs

At top of file are 2 FsCheckConfig bindings one with and one without setting arbitrary list.

Commenting out the test that takes binding with arbitrary allows the adapter to discover tests.

