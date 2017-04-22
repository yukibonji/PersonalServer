Visual Studio test discovery results in message: 

'vstest.discoveryengine.x86.exe' WCF communication channel unavailable. Active request will be aborted.

(See output window, Tests)

But tests have no problem running in the console app.

Cause: appears to be problem with passing types in the FsCheckConfig.arbitrary field

Reproduction steps:

File: PersonalServer\tests\PersonalServer.Tests\DomainTypes.fs

At top of file is FsCheckConfig binding both with and without setting arbitrary list.

