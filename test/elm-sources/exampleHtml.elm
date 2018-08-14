import Html exposing (ul, li, a, text)
import Html.Attributes exposing (attribute, class, href)

view = 
    ul [ class "d-lg-flex pl-lg-2 flex-items-center text-bold list-style-none", attribute "role" "navigation" ]
        [ li []
            [ a [ class "js-selected-navigation-item HeaderNavlink px-lg-2 py-2 py-lg-0", attribute "data-hotkey" "g p", attribute "data-ga-click" "Header, click, Nav menu - item:pulls context:user", attribute "aria-label" "Pull requests you created", attribute "data-selected-links" "/pulls /pulls/assigned /pulls/mentioned /pulls", href "/pulls" ]
                [ text "Pull\nrequests"
                ]
            ]
        , li []
            [ a [ class "js-selected-navigation-item HeaderNavlink px-lg-2 py-2 py-lg-0", attribute "data-hotkey" "g i", attribute "data-ga-click" "Header, click, Nav menu - item:issues context:user", attribute "aria-label" "Issues you created", attribute "data-selected-links" "/issues /issues/assigned /issues/mentioned /issues", href "/issues" ]
                [ text "Issues"
                ]
            ]
        , li []
            [ a [ class "js-selected-navigation-item HeaderNavlink px-lg-2 py-2 py-lg-0", attribute "data-ga-click" "Header, click, Nav menu - item:marketplace context:user", attribute "data-octo-click" "marketplace_click", attribute "data-octo-dimensions" "location:nav_bar", attribute "data-selected-links" "/marketplace", href "/marketplace" ]
                [ text "Marketplace"
                ]
            ]
        , li []
            [ a [ class "js-selected-navigation-item HeaderNavlink px-lg-2 py-2 py-lg-0", attribute "data-ga-click" "Header, click, Nav menu - item:explore", attribute "data-selected-links" "/explore /trending /trending/developers /integrations /integrations/feature/code /integrations/feature/collaborate /integrations/feature/ship showcases showcases_search showcases_landing /explore", href "/explore" ]
                [ text "Explore"
                ]
            ]
        ]
