package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentRegistry;
import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.validation.client.ClientValidator;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class ConcernComponentMock extends Component implements HasAttribute<ConcernComponentMock>,
    HasClassName<ConcernComponentMock>, HasProperty<ConcernComponentMock>,
    HasStyle<ConcernComponentMock>, HasText<ConcernComponentMock>, HasHtml<ConcernComponentMock>,
    HasVisibility<ConcernComponentMock>, HasComponents, HasEnablement<ConcernComponentMock>,
    HasLabel<ConcernComponentMock>, HasMax<ConcernComponentMock, Double>,
    HasMaxLength<ConcernComponentMock>, HasMin<ConcernComponentMock, Double>,
    HasMinLength<ConcernComponentMock>, HasPlaceholder<ConcernComponentMock>,
    HasReadOnly<ConcernComponentMock>, HasTooltip<ConcernComponentMock>,
    HasValue<ConcernComponentMock, Double>, HasClientValidation<ConcernComponentMock>,
    HasClientValidationStyle<ConcernComponentMock>, HasClientAutoValidation<ConcernComponentMock>,
    HasClientAutoValidationOnLoad<ConcernComponentMock>, HasRequired<ConcernComponentMock> {

  private Map<String, String> attributes = new HashMap<>();
  private Map<String, Object> properties = new HashMap<>();
  private String className;
  private String style;
  private String text;
  private String html;
  private boolean visible = true;
  private boolean enabled = true;
  private String label;
  private Double max;
  private int maxLength;
  private Double min;
  private int minLength;
  private String placeholder;
  private boolean readOnly;
  private String tooltip;
  private Double value;
  private ComponentRegistry registry = new ComponentRegistry(this, e -> {
  });
  private boolean isInvalid = false;
  private String invalidMessage;
  private ClientValidator clientValidator;
  private ValidationStyle validationStyle = ValidationStyle.POPOVER;
  private boolean autoValidate = true;
  private boolean autoValidateOnLoad = false;
  private boolean required = false;

  @Override
  public String getAttribute(String attribute) {
    return attributes.get(attribute);
  }

  @Override
  public ConcernComponentMock setAttribute(String attribute, String value) {
    attributes.put(attribute, value);
    return this;
  }

  @Override
  public ConcernComponentMock removeAttribute(String attribute) {
    attributes.remove(attribute);
    return this;
  }

  @Override
  public ConcernComponentMock addClassName(String... classNames) {
    this.className = classNames[0];
    return this;
  }

  public String getClassName() {
    return this.className;
  }

  @Override
  public ConcernComponentMock removeClassName(String... classNames) {
    this.className = null;
    return this;
  }

  @Override
  public ConcernComponentMock setProperty(String property, Object value) {
    properties.put(property, value);
    return this;
  }

  @Override
  public <V> V getProperty(String property, Type typeOfV) {
    return (V) properties.get(property);
  }

  @Override
  public ConcernComponentMock setStyle(String property, String value) {
    this.style = value;
    return this;
  }

  @Override
  public String getStyle(String property) {
    return this.style;
  }

  @Override
  public String getComputedStyle(String property) {
    return this.style;
  }

  @Override
  public ConcernComponentMock removeStyle(String property) {
    this.style = null;
    return this;
  }

  @Override
  public ConcernComponentMock setText(String text) {
    this.text = text;
    return this;
  }

  @Override
  public String getText() {
    return this.text;
  }

  @Override
  public ConcernComponentMock setHtml(String html) {
    this.html = html;
    return this;
  }

  @Override
  public String getHtml() {
    return this.html;
  }

  @Override
  public boolean isVisible() {
    return this.visible;
  }

  @Override
  public ConcernComponentMock setVisible(boolean visible) {
    this.visible = visible;
    return this;
  }

  @Override
  public void add(Component... components) {
    registry.add(components);
  }

  @Override
  public void remove(Component... components) {
    registry.remove(components);
  }

  @Override
  public void removeAll() {
    registry.removeAll();
  }

  @Override
  public boolean hasComponent(Component component) {
    return registry.hasComponent(component);
  }

  @Override
  public List<Component> getComponents() {
    return registry.getComponents();
  }

  @Override
  public int getComponentCount() {
    return registry.getComponentCount();
  }

  @Override
  public Component getComponent(String id) {
    return registry.getComponent(id);
  }

  @Override
  public boolean isEnabled() {
    return this.enabled;
  }

  @Override
  public ConcernComponentMock setEnabled(boolean enabled) {
    this.enabled = enabled;
    return this;
  }

  @Override
  public String getLabel() {
    return this.label;
  }

  @Override
  public ConcernComponentMock setLabel(String label) {
    this.label = label;
    return this;
  }

  @Override
  public Double getMax() {
    return this.max;
  }

  @Override
  public ConcernComponentMock setMax(Double max) {
    this.max = max;
    return this;
  }

  @Override
  public ConcernComponentMock setMaxLength(int maxLength) {
    this.maxLength = maxLength;
    return this;
  }

  @Override
  public int getMaxLength() {
    return this.maxLength;
  }

  @Override
  public Double getMin() {
    return this.min;
  }

  @Override
  public ConcernComponentMock setMin(Double min) {
    this.min = min;
    return this;
  }

  @Override
  public int getMinLength() {
    return this.minLength;
  }

  @Override
  public ConcernComponentMock setMinLength(int minLength) {
    this.minLength = minLength;
    return this;
  }

  public ConcernComponentMock setPlaceholder(String placeholder) {
    this.placeholder = placeholder;
    return this;
  }

  public String getPlaceholder() {
    return this.placeholder;
  }

  @Override
  public ConcernComponentMock setReadOnly(boolean readOnly) {
    this.readOnly = readOnly;
    return this;
  }

  @Override
  public boolean isReadOnly() {
    return this.readOnly;
  }

  @Override
  public String getTooltipText() {
    return this.tooltip;
  }

  @Override
  public ConcernComponentMock setTooltipText(String tooltipText) {
    this.tooltip = tooltipText;
    return this;
  }

  @Override
  public Double getValue() {
    return this.value;
  }

  @Override
  public ConcernComponentMock setValue(Double value) {
    this.value = value;
    return this;
  }

  @Override
  public ConcernComponentMock setInvalid(boolean invalid) {
    this.isInvalid = invalid;
    return this;
  }

  @Override
  public boolean isInvalid() {
    return this.isInvalid;
  }

  @Override
  public ConcernComponentMock setInvalidMessage(String message) {
    this.invalidMessage = message;
    return this;
  }

  @Override
  public String getInvalidMessage() {
    return this.invalidMessage;
  }

  @Override
  public ConcernComponentMock setClientValidator(ClientValidator clientValidator) {
    this.clientValidator = clientValidator;
    return this;
  }

  @Override
  public ClientValidator getClientValidator() {
    return this.clientValidator;
  }

  @Override
  public ValidationStyle getValidationStyle() {
    return validationStyle;
  }

  @Override
  public ConcernComponentMock setValidationStyle(ValidationStyle validationStyle) {
    this.validationStyle = validationStyle;
    return this;
  }

  @Override
  public ConcernComponentMock setAutoClientValidate(boolean autoValidate) {
    this.autoValidate = autoValidate;
    return this;
  }

  @Override
  public boolean isAutoClientValidate() {
    return this.autoValidate;
  }

  @Override
  public ConcernComponentMock setAutoClientValidateOnLoad(boolean autoValidateOnLoad) {
    this.autoValidateOnLoad = autoValidateOnLoad;
    return this;
  }

  @Override
  public boolean isAutoClientValidateOnLoad() {
    return this.autoValidateOnLoad;
  }

  @Override
  public ConcernComponentMock setRequired(boolean required) {
    this.required = required;
    return this;
  }

  @Override
  public boolean isRequired() {
    return this.required;
  }

  @Override
  protected void onCreate(Window window) {
    // pass
  }

  @Override
  protected void onDestroy() {
    // pass
  }

  @Override
  public ListenerRegistration<ValueChangeEvent<Double>> addValueChangeListener(
      EventListener<ValueChangeEvent<Double>> listener) {
    throw new UnsupportedOperationException("Unimplemented method 'addValueChangeListener'");
  }
}
