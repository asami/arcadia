package arcadia.view

import java.util.Locale
import org.joda.time.DateTimeZone
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.context.FormatContext
import org.goldenport.context.DateTimeContext
import org.goldenport.record.v3.{IRecord, Record}
import arcadia._
import arcadia.context.{ExecutionContext, PlatformContext, PlatformExecutionContext}
import arcadia.model.Model
import arcadia.service.ServiceFacility

/*
 * @since   Aug. 15, 2026
 * @version Aug. 15, 2026
 * @author  ASAMI, Tomoharu
 */
final class ViewEngineFormatterSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "ViewEngine formatter precedence" should {
    "preserve a supplied render formatter when parcel context differs" in {
      Given("a parcel render strategy and execution context with distinct format contexts")
      val suppliedcontext = FormatContext.create(Locale.JAPAN, DateTimeZone.forID("Asia/Tokyo"))
      val executioncontext = _execution_context(Locale.US, DateTimeZone.forID("America/New_York"))
      val suppliedformatter = FormatterContext(suppliedcontext)
      val model = new CaptureModel
      val parcel = Parcel(model, PlainHtml.withFormatter(suppliedformatter)).copy(context = Some(executioncontext))

      When("ViewEngine applies its public render interface")
      _view_engine.applyOption(parcel)

      Then("the model receives the caller-supplied formatter unchanged")
      (model.renderStrategy.formatter eq suppliedformatter) shouldBe true
      (model.renderStrategy.formatter.formatContext eq suppliedcontext) shouldBe true
    }

    "use the execution context formatter when parcel render is absent" in {
      Given("a parcel with an execution context and no render strategy")
      val executioncontext = _execution_context(Locale.JAPAN, DateTimeZone.forID("Asia/Tokyo"))
      val model = new CaptureModel
      val parcel = Parcel(model = Some(model), context = Some(executioncontext))

      When("ViewEngine applies its public render interface")
      _view_engine.applyOption(parcel)

      Then("the model receives the formatter derived from that execution context")
      model.renderStrategy.formatter shouldBe FormatterContext.create(executioncontext)
    }

    "use the default formatter when parcel has neither render nor execution context" in {
      Given("a parcel without a render strategy or execution context")
      val model = new CaptureModel
      val parcel = Parcel(model = Some(model))

      When("ViewEngine applies its public render interface")
      _view_engine.applyOption(parcel)

      Then("the model receives Arcadia's default formatter")
      model.renderStrategy.formatter shouldBe FormatterContext.default
    }
  }

  private val _platform = PlatformContext.develop
  private val _application = WebApplication(
    "formatter",
    None,
    WebApplicationConfig.empty,
    arcadia.controller.ControllerEngine.Rule.empty,
    ViewEngine.Rule.error,
    arcadia.domain.DomainModel.empty,
    None
  )
  private val _view_engine = new ViewEngine(
    _platform,
    ViewEngine.Rule.error,
    Nil,
    TemplateEngineHangar.empty
  )

  private def _execution_context(locale: Locale, timezone: DateTimeZone): ExecutionContext = {
    val formatcontext = FormatContext.create(locale, timezone)
    val platformexecutioncontext = new PlatformExecutionContext.StandalonePlatformExecutionContext(
      _platform,
      locale,
      DateTimeContext.now,
      formatcontext
    )
    ExecutionContext(
      platformexecutioncontext,
      new ServiceFacility(_platform, Nil),
      _application
    )
  }

  private class CaptureModel extends Model {
    private var _render_strategy: Option[RenderStrategy] = None

    def renderStrategy: RenderStrategy = _render_strategy.get
    def expiresKind = None
    def toRecord: IRecord = Record.empty
    def render(strategy: RenderStrategy) = {
      _render_strategy = Some(strategy)
      scala.xml.Text("formatter")
    }
  }
}
