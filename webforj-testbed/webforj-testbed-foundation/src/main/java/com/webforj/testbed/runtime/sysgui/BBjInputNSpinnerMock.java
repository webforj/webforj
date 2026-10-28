package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.util.common.BasisNumber;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * The mock backing the number spinner, which applies its step, mask precision and bounds.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputNSpinnerMock extends BBjInputNMock {
  private static final String STEP_PROPERTY = "step";
  private static final String MINIMUM_PROPERTY = "min";
  private static final String MAXIMUM_PROPERTY = "max";
  private static final double DEFAULT_STEP = 1;
  // The browser prints a number in exponent notation from these magnitudes on.
  private static final double EXPONENT_FROM = 1e21;
  private static final double EXPONENT_BELOW = 1e-6;
  private final BBjSpinnerState spinner = new BBjSpinnerState(this::putText, this::step);
  private double stepSize = DEFAULT_STEP;
  private Double minimum;
  private Double maximum;

  BBjInputNSpinnerMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    Object result = spinner.handle(method.getName(), args);
    if (result != BBjSpinnerState.UNHANDLED_SENTINEL) {
      return result;
    }

    // The runtime keeps what its own setters received and hands the browser a property, so its
    // getters answer independently of a property the component writes directly.
    return switch (method.getName()) {
      case "getStepSize" -> BasisNumber.valueOf(stepSize);
      case "setStepSize" -> {
        stepSize = args[0] == null ? DEFAULT_STEP : ((BBjNumber) args[0]).doubleValue();
        getProperties().put(STEP_PROPERTY, Double.toString(stepSize));
        yield null;
      }
      case "getMinimum" -> minimum == null ? null : BasisNumber.valueOf(minimum);
      case "setMinimum" -> {
        minimum = args[0] == null ? null : ((BBjNumber) args[0]).doubleValue();
        getProperties().put(MINIMUM_PROPERTY,
            Double.toString(minimum == null ? -Double.MAX_VALUE : minimum));
        yield null;
      }
      case "getMaximum" -> maximum == null ? null : BasisNumber.valueOf(maximum);
      case "setMaximum" -> {
        maximum = args[0] == null ? null : ((BBjNumber) args[0]).doubleValue();
        getProperties().put(MAXIMUM_PROPERTY,
            Double.toString(maximum == null ? Double.MAX_VALUE : maximum));
        yield null;
      }
      default -> super.handle(proxy, method, args);
    };
  }

  // The browser computes the spin: it steps by the size of the step whatever its sign, rounds to
  // the decimals of the mask, refuses to go below zero when negative numbers are not allowed and
  // stops at the bound in the direction of the spin.
  private void step(boolean up) throws BBjException {
    double value = getNumberValue().doubleValue();
    double step = Math.abs(property(STEP_PROPERTY, DEFAULT_STEP));
    // The browser rounds the binary number itself, not its shortest decimal text, so 0.145 with
    // two decimals becomes 0.14. BigDecimal.valueOf reads the decimal text and gives 0.15.
    double next = new BigDecimal(value + (up ? step : -step)) // NOSONAR
        .setScale(decimalPlaces(), RoundingMode.HALF_UP).doubleValue();
    if (!up && next < 0 && !getProxy(BBjInputNSpinner.class).getNegateable()) {
      return;
    }

    next = up ? Math.min(next, property(MAXIMUM_PROPERTY, Double.POSITIVE_INFINITY))
        : Math.max(next, property(MINIMUM_PROPERTY, Double.NEGATIVE_INFINITY));
    putText(numberText(next));
  }

  private String numberText(double value) {
    BigDecimal decimal = BigDecimal.valueOf(value).stripTrailingZeros();
    double magnitude = Math.abs(value);

    return magnitude >= EXPONENT_FROM || (magnitude > 0 && magnitude < EXPONENT_BELOW)
        ? decimal.toString().replace('E', 'e')
        : decimal.toPlainString();
  }

  private double property(String name, double fallback) {
    String value = getProperties().get(name);

    return value == null || "null".equals(value) ? fallback : Double.parseDouble(value);
  }

  // Only a mask with exactly one decimal point has decimals.
  private int decimalPlaces() {
    String mask = getMask();
    String[] parts = mask == null ? new String[0] : mask.split("\\.", -1);

    return parts.length == 2
        ? (int) parts[1].chars().filter(character -> character == '#' || character == '0').count()
        : 0;
  }
}
