#pragma once

#include <vector>

#include <SDL_vulkan.h>
#include <vulkan/vulkan.h>

auto get_layer_properties() 
    -> std::vector<VkLayerProperties>;

auto get_extension_names(SDL_Window* window) 
    -> std::vector<char const*>;

auto get_physical_devices(VkInstance vk_instance) 
    -> std::vector<VkPhysicalDevice>;

auto get_physical_device_extension_properties(VkPhysicalDevice device)
    -> std::vector<VkExtensionProperties>;

auto get_queue_family_properties(VkPhysicalDevice device) 
    -> std::vector<VkQueueFamilyProperties>;

auto get_physical_device_surface_formats(VkPhysicalDevice device, VkSurfaceKHR surface)
    -> std::vector<VkSurfaceFormatKHR>;

auto get_physical_device_present_modes(VkPhysicalDevice device, VkSurfaceKHR surface)
    -> std::vector<VkPresentModeKHR>;

struct SwapChainDetails {
    VkSurfaceCapabilitiesKHR        caps;
    std::vector<VkSurfaceFormatKHR> formats;
    std::vector<VkPresentModeKHR>   present_modes;
};

auto get_swap_chain_details(VkPhysicalDevice device, VkSurfaceKHR surface)
    -> SwapChainDetails;

auto create_shader_module(VkDevice device, std::vector<char> const& binary)
    -> VkShaderModule;
