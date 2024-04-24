package com.webforj.data.binding;

import com.webforj.data.concern.ReadOnlyAware;
import com.webforj.data.concern.RequiredAware;
import com.webforj.data.concern.ValueAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.validation.InvalidAware;
import com.webforj.data.validation.client.AutoClientValidation;
import com.webforj.data.validation.client.AutoClientValidationOnLoad;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

class NameComponentMock
    implements ValueAware<NameComponentMock, String>, AutoClientValidation<NameComponentMock>,
    InvalidAware<NameComponentMock>, AutoClientValidationOnLoad<NameComponentMock>,
    ReadOnlyAware<NameComponentMock>, RequiredAware<NameComponentMock> {
  private EventDispatcher dispatcher = new EventDispatcher();
  private String value = "";
  private boolean autoClientValidate = true;
  private boolean autoClientValidateOnLoad = true;
  private boolean readOnly = false;
  private boolean required = false;
  private boolean invalid = false;
  private String invalidMessage = "";

  public NameComponentMock(String value) {
    setValue(value);
  }

  public NameComponentMock() {
    this("");
  }

  @Override
  public String getValue() {
    return value;
  }

  @Override
  public NameComponentMock setValue(String value) {
    this.value = value;
    dispatcher.dispatchEvent(new ValueChangeEvent<>(this, value));
    return this;
  }

  @Override
  public ListenerRegistration<ValueChangeEvent<String>> addValueChangeListener(
      EventListener<ValueChangeEvent<String>> listener) {
    return dispatcher.addListener(ValueChangeEvent.class, listener);
  }

  @Override
  public NameComponentMock setInvalid(boolean invalid) {
    this.invalid = invalid;
    return this;
  }

  @Override
  public boolean isInvalid() {
    return invalid;
  }

  @Override
  public NameComponentMock setInvalidMessage(String message) {
    this.invalidMessage = message;
    return this;
  }

  @Override
  public String getInvalidMessage() {
    return invalidMessage;
  }

  @Override
  public NameComponentMock setAutoClientValidate(boolean autoValidate) {
    this.autoClientValidate = autoValidate;
    return this;
  }

  @Override
  public boolean isAutoClientValidate() {
    return autoClientValidate;
  }

  @Override
  public NameComponentMock setAutoClientValidateOnLoad(boolean autoValidateOnLoad) {
    this.autoClientValidateOnLoad = autoValidateOnLoad;
    return this;
  }

  @Override
  public boolean isAutoClientValidateOnLoad() {
    return autoClientValidateOnLoad;
  }

  @Override
  public NameComponentMock setReadOnly(boolean readOnly) {
    this.readOnly = readOnly;
    return this;
  }

  @Override
  public boolean isReadOnly() {
    return readOnly;
  }

  @Override
  public NameComponentMock setRequired(boolean required) {
    this.required = required;
    return this;
  }

  @Override
  public boolean isRequired() {
    return required;
  }
}
