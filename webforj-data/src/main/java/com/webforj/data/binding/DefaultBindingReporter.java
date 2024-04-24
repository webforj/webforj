package com.webforj.data.binding;

import com.webforj.data.concern.ValueAware;
import com.webforj.data.validation.InvalidAware;
import com.webforj.data.validation.server.ValidationResult;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * A default implementation of the {@link BindingReporter} interface.
 *
 * @param <C> the type of the UI component implementing {@link ValueAware}, capable of displaying
 *        and updating a value of type V.
 * @param <B> the type of the bean from which the value is being synchronized.
 * @param <BV> the type of the value that is being bound.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class DefaultBindingReporter<C extends ValueAware<C, CV>, CV, B, BV>
    implements BindingReporter<C, CV, B, BV> {
  private Function<List<String>, String> formatter;

  /**
   * Creates a new binding reporter.
   */
  public DefaultBindingReporter(Function<List<String>, String> formatter) {
    setFormatter(formatter);
  }

  /**
   * Creates a new binding reporter.
   */
  public DefaultBindingReporter() {
    setFormatter(msgs -> msgs.size() > 1 ? msgs.stream().map(message -> "<li>" + message + "</li>")
        .collect(Collectors.joining("", "<ol>", "</ol>")) : msgs.get(0));
  }

  /**
   * Sets the function to format the validation messages.
   *
   * @param formatter the formatter function.
   */
  public void setFormatter(Function<List<String>, String> formatter) {
    this.formatter = formatter;
  }

  /**
   * Gets the function to format the validation messages.
   *
   * @return the formatter function.
   */
  public Function<List<String>, String> getFormatter() {
    return formatter;
  }

  @Override
  public void report(ValidationResult validateResult, Binding<C, CV, B, BV> binding) {
    C component = binding.getComponent();

    if (component instanceof InvalidAware) {
      @SuppressWarnings("unchecked")
      InvalidAware<C> iac = (InvalidAware<C>) component;
      boolean isValid = validateResult.isValid();
      iac.setInvalid(!isValid);

      if (!isValid) {
        List<String> messages = validateResult.getMessages();
        if (!messages.isEmpty()) {
          String htmlMessage = Optional.ofNullable(formatter).map(f -> f.apply(messages))
              .orElse(messages.stream().collect(Collectors.joining("<br>")));
          iac.setInvalidMessage(htmlMessage);
        }
      } else {
        iac.setInvalidMessage("");
      }
    }
  }
}
