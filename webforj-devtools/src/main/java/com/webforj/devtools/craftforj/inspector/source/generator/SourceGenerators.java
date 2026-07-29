package com.webforj.devtools.craftforj.inspector.source.generator;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.KeyValueConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.ListConcernContribution;

/**
 * Selects the {@link SourceGenerator} that matches a feature handler's contribution shape.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SourceGenerators {

  private SourceGenerators() {}

  /**
   * Selects the generator for the given handler, honoring a handler-provided override first.
   *
   * @param handler the feature handler the change belongs to
   * @param component the live component, or {@code null} when it was destroyed
   * @return the generator producing the handler's source changes
   */
  public static SourceGenerator select(FeatureHandler handler, Component component) {
    SourceGenerator custom = handler.getSourceGenerator();
    if (custom != null) {
      return custom;
    }

    if (handler instanceof ListConcernContribution<?>) {
      return new ListSourceGenerator();
    }
    if (handler instanceof KeyValueConcernContribution<?>) {
      return new KeyValueSourceGenerator();
    }
    if (handler instanceof EnumConcernContribution<?> enumHandler) {
      // Resolve the enum class from the live component so the client value never drives class
      // loading; for destroyed components the generator loads without initialization
      Class<?> enumClass = component != null ? enumHandler.findEnumClass(component) : null;
      return new EnumSourceGenerator(enumClass);
    }

    return new ScalarSourceGenerator();
  }
}
