package org.mule.scheletor
import org.mule.scheletor.ErrorType._
import org.mule.scheletor.FormatValidator._
import org.scalatest.{FunSuite, Matchers, OptionValues}

trait FormatValidatorTest extends FunSuite with Matchers with OptionValues {

  test("dateTimeExceedingLimits") {
    DateTime("1996-60-999T16:39:57-08:00").value shouldBe DateTimeError("1996-60-999T16:39:57-08:00")
  }

  test("dateTimeFormatFailure") {
    DateTime("2015-03-13T11:00:000").value shouldBe DateTimeError("2015-03-13T11:00:000")
  }

  test("dateTimeWithSingleDigitInSecFracSuccess") { DateTime("2015-02-28T11:00:00.1Z") shouldBe empty }

  test("dateTimeWithTwoDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.12Z") shouldBe empty }

  test("dateTimeWithThreeDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.123Z") shouldBe empty }

  test("dateTimeWithFourDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.1234Z") shouldBe empty }

  test("dateTimeWithFiveDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.12345Z") shouldBe empty }

  test("dateTimeWithSixDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.123456Z") shouldBe empty }

  test("dateTimeWithSevenDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.1234567Z") shouldBe empty }

  test("dateTimeWithEightDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.12345678Z") shouldBe empty }

  test("dateTimeWithNineDigitsInSecFracSuccess") { DateTime("2015-02-28T11:00:00.123456789Z") shouldBe empty }

  test("dateTimeWithTenDigitsInSecFracFailure") {
    DateTime("2015-02-28T11:00:00.1234567890Z").value shouldBe DateTimeError("2015-02-28T11:00:00.1234567890Z")
  }

  test("dateTimeSuccess") { DateTime("2015-03-13T11:00:00+00:00") shouldBe empty }

  test("dateTimeZSuccess") { DateTime("2015-02-28T11:00:00Z") shouldBe empty }

  test("dateFailure") { Date("06/19/1963").value shouldBe DateError("06/19/1963") }

  test("dateSuccess") { Date("1963-06-19") shouldBe empty }

  test("timeFailure") { Time("08:30:06 PST").value shouldBe TimeError("08:30:06 PST") }

  test("timeSuccess") { Time("11:00:00+00:00") shouldBe empty }

  test("timeZSuccess") { Time("11:00:00Z") shouldBe empty }

  test("timeWithSingleDigitInSecFracSuccess") { Time("11:00:00.1Z") shouldBe empty }

  test("timeWithTwoDigitsInSecFracSuccess") { Time("11:00:00.12Z") shouldBe empty }

  test("timeWithThreeDigitsInSecFracSuccess") { Time("11:00:00.123Z") shouldBe empty }

  test("timeWithFourDigitsInSecFracSuccess") { Time("11:00:00.1234Z") shouldBe empty }

  test("timeWithFiveDigitsInSecFracSuccess") { Time("11:00:00.12345Z") shouldBe empty }

  test("timeWithSixDigitsInSecFracSuccess") { Time("11:00:00.123456Z") shouldBe empty }

  test("timeWithSevenDigitsInSecFracSuccess") { Time("11:00:00.1234567Z") shouldBe empty }

  test("timeWithEightDigitsInSecFracSuccess") { Time("11:00:00.12345678Z") shouldBe empty }

  test("timeWithNineDigitsInSecFracSuccess") { Time("11:00:00.123456789Z") shouldBe empty }

  test("timeWithTenDigitsInSecFracFailure") {
    Time("11:00:00.1234567890Z").value shouldBe TimeError("11:00:00.1234567890Z")
  }

  test("emailFailure") { Email("a.@b.com").value shouldBe EmailError("a.@b.com") }

  test("emailSuccess") { Email("a@b.com") shouldBe empty }

  test("hostnameLengthFailure") {
    val sb = "a" * 256
    HostName(sb).value shouldBe HostNameError(sb)
  }

  test("hostnameNullFailure") { HostName(null).value shouldBe HostNameError(null) }

  test("hostnameSuccess") { HostName("localhost") shouldBe empty }

  test("hostnameWithUnderscoresFailure") {
    HostName("not_a_valid_host_name").value shouldBe HostNameError("not_a_valid_host_name")
  }

  test("ipv4Failure") { IPv4("asd").value shouldBe IPv4Error("asd") }

  test("ipv4LengthFailure") { IPv4(IPv6Addr).value shouldBe IPv4Error(IPv6Addr) }

  test("ipv4NullFailure") { IPv4(null).value shouldBe IPv4Error(null) }

  test("ipv4Success") { IPv4(ThereIsNoPlaceLike) shouldBe empty }

  test("ipv6Failure") { IPv6("asd").value shouldBe IPv6Error("asd") }

  test("ipv6LengthFailure") { IPv6(ThereIsNoPlaceLike).value shouldBe IPv6Error(ThereIsNoPlaceLike) }

  test("ipv6NullFailure") { IPv6(null).value shouldBe IPv6Error(null) }

  test("ipv6Success") { IPv6(IPv6Addr) shouldBe empty }

  test("uriFailure") { URI("12 34").value shouldBe UriError("12 34") }

  test("relativeURIFailure") {
    URI("//abc").value shouldBe UriError("abc")
    URIReference("//abc") shouldBe empty
    URIReference("//foo.bar/?baz=qux#quux") shouldBe empty
  }

  test("uriNullFailure") { URI(null).value shouldBe UriError(null) }

  test("uriSuccess") { URI("http://example.org:8080/example.html") shouldBe empty }

  test("uriTemplateSuccess") { URITemplate("http://example.org:8080/example{aa}.html") shouldBe empty }

  test("uriTemplateFailure") {
    val s = "http://example.org:8080/example{>aa}.html"
    URITemplate(s).value shouldBe UriTemplateError(s)
  }

  test("regexSuccess") { Regex("([abc])+\\s+$") shouldBe empty }

  test("regexFailure") { Regex("^(abc]").value shouldBe RegexError("^(abc]") }

  test("null regex") { Regex(null).value shouldBe RegexError(null) }

  private final val IPv6Addr           = "2001:db8:85a3:0:0:8a2e:370:7334"
  private final val ThereIsNoPlaceLike = "127.0.0.1"

}
