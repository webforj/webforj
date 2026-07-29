package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasAttribute;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the column span of a columns layout item, applied via
 * {@code ColumnsLayout.setSpan}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutItemSpanContribution extends ColumnsLayoutItemContribution {

  private static final String ATTRIBUTE = "data-span";

  /** Creates the columns layout item span contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public ColumnsLayoutItemSpanContribution() {
    super(HasAttribute.class, "Span", "setSpan");

    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(c -> parseInt(((HasAttribute<?>) c).getAttribute(ATTRIBUTE)));
    setSetter((parent, item, v) -> parent.setSpan((Component & HasAttribute) item, toInt(v)));
    setResetter((parent, item) -> ((HasAttribute<?>) item).removeAttribute(ATTRIBUTE));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ValueExpression toSourceExpression(FeatureProperty property) {
    return scalarExpression(property);
  }
}
