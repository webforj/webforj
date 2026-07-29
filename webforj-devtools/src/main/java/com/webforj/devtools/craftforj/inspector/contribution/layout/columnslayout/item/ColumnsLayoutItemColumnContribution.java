package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasAttribute;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the column placement of a columns layout item, applied via
 * {@code ColumnsLayout.setColumn}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutItemColumnContribution extends ColumnsLayoutItemContribution {

  private static final String ATTRIBUTE = "data-column";

  /** Creates the columns layout item column contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public ColumnsLayoutItemColumnContribution() {
    super(HasAttribute.class, "Column", "setColumn");

    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(c -> parseInt(((HasAttribute<?>) c).getAttribute(ATTRIBUTE)));
    setSetter((parent, item, v) -> parent.setColumn((Component & HasAttribute) item, toInt(v)));
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
