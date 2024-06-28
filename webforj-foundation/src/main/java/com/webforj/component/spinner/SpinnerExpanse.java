package com.webforj.component.spinner;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.ExpanseBase;

/**
 * Supported expanses as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public enum SpinnerExpanse implements ExpanseBase {
  /* The 3xlarge expanse as defined by the Dwc Theme Engine. */
  @SerializedName("3xl")
  XXXLARGE,

  /* The 2xlarge expanse as defined by the Dwc Theme Engine. */
  @SerializedName("2xl")
  XXLARGE,

  /* The xlarge expanse as defined by the Dwc Theme Engine. */
  @SerializedName("xl")
  XLARGE,

  /* The large expanse as defined by the Dwc Theme Engine. */
  @SerializedName("l")
  LARGE,

  /* The medium expanse as defined by the Dwc Theme Engine. */
  @SerializedName("m")
  MEDIUM,

  /* The small expanse as defined by the Dwc Theme Engine. */
  @SerializedName("s")
  SMALL,

  /* The xsmall expanse as defined by the Dwc Theme Engine. */
  @SerializedName("xs")
  XSMALL,

  /* The 2xsmall expanse as defined by the Dwc Theme Engine. */
  @SerializedName("2xs")
  XXSMALL,

  /* The 3xsmall expanse as defined by the Dwc Theme Engine. */
  @SerializedName("3xs")
  XXXSMALL,

  /* No expanse is applied. */
  @SerializedName("")
  NONE;
}
