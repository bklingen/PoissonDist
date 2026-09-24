# Startup promo for the Art of Stat mobile app.
# This file is not copied by sync_sibling.R, so the Lumen app does not get it.
# server.R sources it only when lumen is FALSE.

show_artofstat_modal <- function() {
  showModal(
    modalDialog(
      title = "Art of Stat Mobile App",
      tags$style(HTML(aos_modal_css())),
      tags$script(HTML(aos_modal_js())),
      tags$div(
        class = "aos-promo",
        tags$div(style = "text-align: center;",
          tags$span(class = "aos-trial", "7-day free trial")
        ),
        tags$p(class = "aos-lead", "All of Art of Stat in a single app"),
        tags$a(
          href = "https://artofstat.com/mobile-apps",
          target = "_blank",
          rel = "noopener",
          tags$img(
            src = "EightAppsinOne2.png",
            class = "aos-hero",
            alt = "Art of Stat modules combined in one app"
          )
        ),
        tags$ul(
          class = "aos-features",
          tags$li("Learn Statistics & Data Science"),
          tags$li("Works offline, no Wi-Fi needed"),
          tags$li("Open CSV files from cloud accounts"),
          # tags$li("No credit card required")
        )
      ),
      footer = tagList(
        tags$div(
          class = "aos-badges",
          tags$a(
            href = "https://apps.apple.com/us/app/art-of-stat/id6755374228",
            target = "_blank",
            rel = "noopener",
            tags$img(src = "badge-app-store.svg", alt = "Download on the App Store")
          ),
          tags$a(
            href = "https://play.google.com/store/apps/details?id=com.artofstat.app",
            target = "_blank",
            rel = "noopener",
            tags$img(src = "badge-google-play.svg", alt = "Get it on Google Play")
          )
        ),
        tags$div(
          class = "aos-actions",
          tags$a(
            class = "btn aos-cta",
            href = "https://artofstat.com/mobile-apps",
            target = "_blank",
            rel = "noopener",
            "More Info"
          ),
          tags$button(
            type = "button",
            class = "btn aos-dismiss",
            `data-dismiss` = "modal",
            `data-bs-dismiss` = "modal",
            "Dismiss"
          )
        )
      ),
      size = "s",
      easyClose = TRUE,
      fade = TRUE
    )
  )
}

aos_modal_css <- function() {
  "
  /* Bootstrap hides the page scrollbar and pads the body while a modal
     is open. That padding changes the sidebar width when the modal
     finally closes. Keep the scrollbar and drop the extra padding. */
  body.modal-open {
    overflow: visible !important;
    padding-right: 0 !important;
  }
  #shiny-modal:has(.aos-promo) .modal-dialog {
    width: 350px;
    max-width: calc(100% - 24px);
    margin: 16px auto;
  }
  #shiny-modal:has(.aos-promo) .modal-content {
    border: none;
    border-radius: 16px;
    overflow: hidden;
    background: #121212;
    box-shadow: 0 24px 60px rgba(0, 0, 0, 0.45);
  }
  #shiny-modal:has(.aos-promo) .modal-header {
    position: relative;
    border-bottom: none;
    padding: 16px 44px 0 44px;
    background: #121212;
    text-align: center;
    display: flex;
    justify-content: center;
  }
  #shiny-modal:has(.aos-promo) .modal-title {
    flex: 1 1 auto;
    margin: 0;
    text-align: center;
    font-family: 'Segoe UI', Arial, sans-serif;
    font-size: 20px;
    font-weight: 700;
    line-height: 1.2;
    color: #ffffff;
    white-space: nowrap;
  }
  #shiny-modal:has(.aos-promo) .modal-header .close {
    position: absolute;
    right: 14px;
    top: 10px;
    float: none;
    margin: 0;
    font-size: 28px;
    font-weight: 300;
    color: #ffffff;
    opacity: 0.85;
    text-shadow: none;
  }
  #shiny-modal:has(.aos-promo) .modal-body {
    padding: 8px 22px 4px;
    background: #121212;
    color: #ffffff;
  }
  #shiny-modal:has(.aos-promo) .modal-footer {
    border-top: none;
    padding: 8px 22px 8px;
    background: #121212;
    display: flex;
    flex-direction: column;
    flex-wrap: nowrap;
    justify-content: flex-start;
    align-items: stretch;
    gap: 10px;
  }
  #shiny-modal:has(.aos-promo) .modal-footer > * {
    margin: 0;
    width: 100%;
  }
  .aos-trial {
    display: inline-block;
    margin: 2px 0 14px;
    padding: 8px 18px;
    border-radius: 22px;
    background: rgba(76, 175, 80, 0.2);
    border: 1px solid #4CAF50;
    color: #4CAF50;
    font-size: 16px;
    font-weight: 700;
    letter-spacing: 0.02em;
  }
  .aos-lead {
    margin: 0 0 14px;
    color: #ffffff;
    font-size: 17px;
    font-weight: 400;
    line-height: 1.35;
    text-align: center;
  }
  .aos-hero {
    display: block;
    width: calc(100% + 28px);
    max-width: none;
    height: auto;
    margin: 0 -14px 14px;
  }
  .aos-features {
    list-style: none;
    margin: 0 0 8px;
    padding: 0;
  }
  .aos-features li {
    display: flex;
    align-items: flex-start;
    gap: 10px;
    margin: 0 0 8px;
    color: #ffffff;
    font-size: 15px;
    line-height: 1.35;
  }
  .aos-features li:before {
    content: '\\2713';
    color: #4CAF50;
    font-size: 18px;
    line-height: 1.1;
  }
  .aos-badges {
    display: flex;
    flex-direction: row;
    justify-content: center;
    align-items: center;
    gap: 8px;
    margin-bottom: 20px;
    transform: translateY(-8px);
  }
  .aos-badges a {
    display: inline-block;
    line-height: 0;
  }
  .aos-badges img {
    height: 46px;
    width: auto;
    display: block;
  }
  .aos-actions {
    display: flex;
    gap: 10px;
  }
  .aos-actions .btn {
    flex: 1;
    border-radius: 12px;
    padding: 10px 12px;
    font-size: 16px;
    font-weight: 700;
  }
  #shiny-modal:has(.aos-promo) .aos-cta {
    background: transparent;
    border: 2px solid #FFD700;
    color: #FFD700;
    box-shadow: none;
    text-align: center;
  }
  #shiny-modal:has(.aos-promo) .aos-cta:hover,
  #shiny-modal:has(.aos-promo) .aos-cta:focus {
    background: transparent;
    border-color: #FFE14A;
    color: #FFE14A;
    text-decoration: none;
  }
  #shiny-modal:has(.aos-promo) .aos-dismiss {
    background: linear-gradient(180deg, #FFD700, #FFA500);
    border: 2px solid #FFD700;
    color: #000000;
    box-shadow: none;
  }
  #shiny-modal:has(.aos-promo) .aos-dismiss:hover,
  #shiny-modal:has(.aos-promo) .aos-dismiss:focus {
    background: linear-gradient(180deg, #FFE14A, #FFB020);
    border-color: #FFE14A;
    color: #000000;
  }
  "
}

aos_modal_js <- function() {
  "
  window.aosPromoTarget = function () {
    var icons = document.querySelectorAll('.sidebar-icon');
    var i, rect;
    for (i = 0; i < icons.length; i++) {
      rect = icons[i].getBoundingClientRect();
      if (rect.width < 2 || rect.height < 2) continue;
      if (rect.bottom > 8 && rect.top < window.innerHeight - 8 &&
          rect.right > 0 && rect.left < window.innerWidth) {
        return {
          x: rect.left + rect.width / 2,
          y: rect.top + rect.height / 2,
          w: rect.width,
          h: rect.height,
          el: icons[i]
        };
      }
    }
    return { x: 36, y: window.innerHeight - 36, w: 85, h: 85, el: null };
  };

  if (!window.aosPromoBound) {
    window.aosPromoBound = true;
    if (!document.getElementById('aos-icon-flash-style')) {
      var flashStyle = document.createElement('style');
      flashStyle.id = 'aos-icon-flash-style';
      flashStyle.textContent = '@keyframes aos-icon-flash {' +
        '0% { transform: scale(1); box-shadow: 0 0 0 0 rgba(66, 165, 245, 0); }' +
        '35% { transform: scale(1.12); box-shadow: 0 0 0 3px #42A5F5, 0 0 16px 5px #1565C0; }' +
        '100% { transform: scale(1); box-shadow: 0 0 0 0 rgba(21, 101, 192, 0); }' +
        '}' +
        '.sidebar-icon.aos-icon-flash { animation: aos-icon-flash 560ms ease-out; }';
      document.head.appendChild(flashStyle);
    }
    $(document).on('hide.bs.modal', '#shiny-modal', function (e) {
      var modal = this;
      if (!modal.querySelector('.aos-promo')) return;
      if (modal.getAttribute('data-aos-closing') === '1') return;
      e.preventDefault();
      modal.setAttribute('data-aos-closing', '1');

      var dialog = modal.querySelector('.modal-dialog');
      var from = dialog.getBoundingClientRect();
      var target = window.aosPromoTarget();
      var dx = target.x - (from.left + from.width / 2);
      var dy = target.y - (from.top + from.height / 2);
      var scaleX = target.w / from.width;
      var scaleY = target.h / from.height;

      modal.style.pointerEvents = 'none';
      dialog.style.transition = 'transform 650ms ease-in-out, opacity 140ms ease-in 770ms';
      dialog.style.transformOrigin = 'center center';
      dialog.style.transform = 'translate(' + dx + 'px, ' + dy + 'px) scale(' + scaleX + ', ' + scaleY + ')';
      dialog.style.opacity = '0';

      var backdrop = document.querySelector('.modal-backdrop');
      if (backdrop) {
        backdrop.style.transition = 'opacity 650ms ease-in';
        backdrop.style.opacity = '0';
      }

      setTimeout(function () {
        if (!target.el) return;
        target.el.classList.remove('aos-icon-flash');
        void target.el.offsetWidth;
        target.el.classList.add('aos-icon-flash');
      }, 760);

      setTimeout(function () {
        if ($.fn.modal) {
          $(modal).modal('hide');
        } else if (window.bootstrap && bootstrap.Modal) {
          bootstrap.Modal.getOrCreateInstance(modal).hide();
        }
      }, 910);
    });
    $(document).on('click', '#shiny-modal .aos-cta', function () {
      var modal = document.getElementById('shiny-modal');
      var backdrop;
      if (!modal || !modal.querySelector('.aos-promo')) return;
      modal.setAttribute('data-aos-closing', '1');
      modal.classList.remove('fade');
      backdrop = document.querySelector('.modal-backdrop');
      if (backdrop) backdrop.classList.remove('fade');
      if ($.fn.modal) {
        $(modal).modal('hide');
      } else if (window.bootstrap && bootstrap.Modal) {
        bootstrap.Modal.getOrCreateInstance(modal).hide();
      }
    });
  }
  "
}
