package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.flexlayout.FlexAlignment;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SelectOption;
import java.util.Arrays;
import java.util.List;

/**
 * Contribution for the self-alignment of a flex item, applied via
 * {@code FlexLayout.setItemAlignment}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexItemAlignmentContribution extends FlexItemContribution {

  private static final List<SelectOption> OPTIONS = Arrays.stream(FlexAlignment.values())
      .map(a -> new SelectOption(FlexAlignment.class.getCanonicalName() + "." + a.name(), a.name()))
      .toList();

  /** Creates the flex item alignment contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public FlexItemAlignmentContribution() {
    super("align-self", "Alignment", "setItemAlignment");

    setBuilderConfig(b -> b.select(OPTIONS));
    setGetter(c -> {
      String value = readStyle(c);
      if (value == null) {
        return null;
      }
      FlexAlignment alignment = FlexAlignment.fromValue(value);

      return alignment == null ? null
          : FlexAlignment.class.getCanonicalName() + "." + alignment.name();
    });
    setSetter((parent, item, v) -> {
      String name = String.valueOf(v);
      FlexAlignment alignment = FlexAlignment.valueOf(name.substring(name.lastIndexOf('.') + 1));
      parent.setItemAlignment(alignment, (Component & HasStyle) item);
    });
    setResetter((parent, item) -> clearStyle(item));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ValueExpression toSourceExpression(FeatureProperty property) {
    return enumExpression(property, FlexAlignment.class);
  }
}
