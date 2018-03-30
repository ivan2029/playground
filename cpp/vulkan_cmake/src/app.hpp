#pragma once

#include <SDL.h>
#include <SDL_vulkan.h>
#include <vulkan/vulkan.h>

#include <cinttypes>
#include <vector>

class App {
public: //

    App(int /*argc*/, char** /*argv*/);

    App() = delete;
    App(App const&) = delete;
    App(App&&) = delete;

    ~App();

    auto operator= (App const& ) -> App& = delete;
    auto operator= (App&& ) -> App& = delete;

public: // 

    auto run() -> void;

private:

    struct QueueFamilyIndices {
        int graphics{-1};
        int present{-1};

        inline
        auto is_complete() const -> bool {
            return graphics >= 0
                && present >= 0
                ;
        }
    };

private: // helpers

    // 
    auto main_loop() -> void;

    auto process_events() -> void;

    //
    auto init_window() -> void;
    auto deinit_window() -> void;

    //
    auto vlk_create_instance() -> void;
    auto vlk_destroy_instance() -> void;

    auto vlk_create_surface() -> void;
    auto vlk_destroy_surface() -> void;

    auto vlk_find_queue_families(VkPhysicalDevice device)
        -> QueueFamilyIndices;

    auto vlk_pick_physical_device() -> void;

    auto vlk_print_physical_device_details() -> void;

    auto vlk_create_device() -> void;
    auto vlk_destroy_device() -> void;

    auto vlk_get_graphics_queue() -> void;
    auto vlk_get_present_queue() -> void;

    auto vlk_create_swap_chain() -> void;
    auto vlk_destroy_swap_chain() -> void;

    auto vlk_get_swap_chain_images() -> void;

    auto vlk_get_swap_chain_image_views() -> void;
    auto vlk_destroy_swap_chain_image_views() -> void;

    auto vlk_print_image_info() -> void;

    auto vlk_create_pipeline() -> void;
    auto vlk_destroy_pipeline() -> void;

    auto vlk_create_shader_modules() -> void;
    auto vlk_destroy_shader_modules() -> void;

private: // fields
    //
    bool running{true};

    int screen_width{800}, screen_height{600};

    // SDL2
    SDL_Window* window{nullptr};

    // Vulkan
    bool                      vlk_validation_layer_enabled{false};

    VkInstance                vlk_instance;
    VkSurfaceKHR              vlk_surface;

    VkPhysicalDevice          vlk_physical_device;
    VkDevice                  vlk_device;
    VkQueue                   vlk_graphics_queue;
    VkQueue                   vlk_present_queue;

    VkSwapchainKHR            vlk_swap_chain;
    VkFormat                  vlk_format;
    VkExtent2D                vlk_extent;
    std::vector<VkImage>      vlk_images;
    std::vector<VkImageView>  vlk_image_views;

    VkShaderModule            vlk_vertex_shader_module;
    VkShaderModule            vlk_fragment_shader_module;
};
