package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.columnslayout.ColumnsLayout.Alignment;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SelectOption;
import java.util.Arrays;
import java.util.List;

/**
 * Contribution for the vertical self-alignment of a columns layout item, applied via
 * {@code ColumnsLayout.setVerticalAlignment(item, alignment)}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutItemVerticalAlignmentContribution extends ColumnsLayoutItemContribution {

  private static final String STYLE_KEY = "align-self";
  private static final List<SelectOption> OPTIONS = Arrays.stream(Alignment.values())
      .map(a -> new SelectOption(Alignment.class.getCanonicalName() + "." + a.name(), a.name()))
      .toList();

  /** Creates the columns layout item vertical alignment contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public ColumnsLayoutItemVerticalAlignmentContribution() {
    super(HasStyle.class, "SelfVerticalAlignment", "setVerticalAlignment");

    setBuilderConfig(b -> b.select(OPTIONS));
    setGetter(c -> readAlignment(c, STYLE_KEY));
    setSetter((parent, item, v) -> parent.setVerticalAlignment((Component & HasStyle) item,
        parseAlignment(v)));
    setResetter((parent, item) -> ((HasStyle<?>) item).setStyle(STYLE_KEY, ""));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ValueExpression toSourceExpression(FeatureProperty property) {
    return enumExpression(property, Alignment.class);
  }
}
