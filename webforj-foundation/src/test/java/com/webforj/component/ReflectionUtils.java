package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.apache.commons.lang3.reflect.FieldUtils;

public class ReflectionUtils {

  public static void nullifyControl(DwcComponent<?> component) throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  public static void unNullifyControl(DwcComponent<?> component, BBjControl control)
      throws IllegalAccessException {
    FieldUtils.writeField(component, "control", control, true);
  }
}
