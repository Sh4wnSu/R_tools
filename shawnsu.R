# ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— #
# Packages loading...
# ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— #
library('vegan')
library('stringr')
library('pheatmap')


# ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— #
# Global variable defining...
# ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— #
# Set the global palette
palette_ss_global <<- c('#428AB3', '#C56577', '#888888', '#4D9D95', '#C38636', '#629A54', '#CD523C',
                        '#82BECB', '#E49CAA', '#B4B4B4', '#89C7C1', '#E9D57F', '#B9D299', '#F7A277',
                        '#215A86', '#9F3F55', '#5A5A5A', '#0F6F66', '#935606', '#4C7532', '#963A20')
n_palette_ss_global <<- length(palette_ss_global)

# Set the global shapes set
shapes_ss_global <<- c(21, 22, 24, 23, 25)
n_shapes_ss_global <<- length(shapes_ss_global)



# ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— #
# Functions loading...
# ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— #
# Filled zero
fillDigits = function(vec_names){
    
    nDigits = ceiling(log10(length(vec_names)))
    
    for(each in vec_names){
        nDigits_each = length(str_extract_all(each, '[0-9]', simplify = T))
        nZeros = nDigits - nDigits_each
        strZeros = ''
        strPattern = ''
        
        if(nZeros != 0){
            for(n in c(1:nZeros)){
                strZeros = paste(strZeros, '0', sep = '')
            }
            for(m in c(1:nDigits_each)){
                strPattern = paste(strPattern, '[0-9]', sep = '')
            }
            vec_names[which(vec_names == each)] = str_replace(each, strPattern, paste(strZeros, str_extract(each, strPattern), sep = ''))
        }
    }
    
    return(vec_names)
}

# Set groups
setGroups1 = function(factor1, row_names, order_fac1, order_col, order_pch){
    
    # Create a group dataframe
    df_groups = data.frame(Factor1 = factor1)
    if(!is.null(row_names)){
        if(length(row_names) == length(factor1)){
            row.names(df_groups) = row_names
        }
        else{
            stop(simpleError("Vectors [factor1] and [row_names] have different lengths"))
        }
    }
    
    # Extract the unique groups
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Set color
    if(is.null(order_col)){
        if(n_fac1 <= n_palette_ss_global){
            vec_col = palette_ss_global
        }
        else{
            warning("The number of groups is greater than the number of colors in palette!
                    Colors will be reused.")
            quotient = floor(n_fac1 / n_palette_ss_global)
            remainder = n_fac1 - quotient * n_palette_ss_global
            vec_col = c(rep(palette_ss_global, quotient), palette_ss_global[1 : remainder])
        }
        
    }
    else if(length(order_col) == 1){
        vec_col = rep(order_col, n_fac1)
    }
    else if(length(order_col) == n_fac1){
        vec_col = order_col
    }
    else{
        stop(simpleError("ERROR: The number of colors in [order_col] is incompatible with the number of groups in [factor1]"))
    }
    
    # Set shape
    if(is.null(order_pch)){
        vec_pch = rep(shapes_ss_global[1], n_fac1)
    }
    else if(length(order_pch) == 1){
        vec_pch = rep(order_pch, n_fac1)
    }
    else if(length(order_pch) == n_fac1){
        vec_pch = order_pch
    }
    else{
        stop(simpleError("ERROR: The number of shapes in [order_pch] is incompatible with the number of groups in [factor1]"))
    }
    
    # Assign color and shape to each element
    for(i in c(1 : n_fac1)){
        df_groups[df_groups[, 'Factor1'] == vec_fac1[i], 'col'] = vec_col[i]
        df_groups[df_groups[, 'Factor1'] == vec_fac1[i], 'pch'] = vec_pch[i]
    }
    
    return(df_groups)
}

setGroups2 = function(factor1, factor2, row_names, order_fac1, order_fac2, order_col, order_pch){
    
    # Judge whether the length of [factor1] and [factor2] are equal
    if(length(factor1) != length(factor2)){
        stop(simpleError("Vectors [factor1] and [factor2] have different lengths"))
    }
    
    # Create a group dataframe
    df_groups = data.frame(Factor1 = factor1, Factor2 = factor2)
    if(!is.null(row_names)){
        if(length(row_names) == length(factor1)){
            row.names(df_groups) = row_names
        }
        else{
            stop(simpleError("Vectors [factor1]/[factor2] have different lengths with [row_names]"))
        }
    }
    
    # Extract the unique groups in factor1
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    # Judge whether the [order_fac1] is compatible with the default
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Extract the unique groups in factor2
    vec_fac2 = sort(unique(factor2))
    n_fac2 = length(vec_fac2)
    # Judge whether the [order_fac2] is compatible with the default
    if(!is.null(order_fac2)){
        if(setequal(order_fac2, vec_fac2)){
            vec_fac2 = order_fac2
        }
        else{
            stop(simpleError("The unique elements in [factor2] are incompitable with [order_fac2]"))
        }
    }
    
    # Set color
    if(is.null(order_col)){
        if(n_fac1 <= n_palette_ss_global){
            vec_col = palette_ss_global
        }
        else{
            warning("The number of groups is greater than the number of colors in palette!
                    Colors will be reused.")
            quotient = floor(n_fac1 / n_palette_ss_global)
            remainder = n_fac1 - quotient * n_palette_ss_global
            vec_col = c(rep(palette_ss_global, quotient), palette_ss_global[1 : remainder])
        }
        
    }
    else if(length(order_col) == 1){
        vec_col = rep(order_col, n_fac1)
    }
    else if(length(order_col) == n_fac1){
        vec_col = order_col
    }
    else{
        stop(simpleError("ERROR: The number of colors in [order_col] is incompatible with the number of groups in [factor1]"))
    }
    
    # Set shape
    if(is.null(order_pch)){
        if(n_fac2 <= n_shapes_ss_global){
            vec_pch = shapes_ss_global
        }
        else{
            warning("The number of groups is greater than the number of default shapes!
                    Shapes will be reused.")
            quotient = floor(n_fac2 / n_shapes_ss_global)
            remainder = n_fac2 - quotient * n_shapes_ss_global
            vec_pch = c(rep(shapes_ss_global, quotient), shapes_ss_global[1 : remainder])
        }
        
    }
    else if(length(order_pch) == 1){
        vec_pch = rep(order_pch, n_fac2)
    }
    else if(length(order_pch) == n_fac2){
        vec_pch = order_pch
    }
    else{
        stop(simpleError("ERROR: The number of shapes in [order_pch] is incompatible with the number of groups in [factor1]"))
    }
    
    for(i in c(1 : n_fac1)){
        for(j in c(1 : n_fac2)){
            df_groups[df_groups[, 'Factor1'] == vec_fac1[i] &
                          df_groups[,  'Factor2'] == vec_fac2[j], 'Factors'] = paste(vec_fac1[i], vec_fac2[j], sep = '_')
            df_groups[df_groups[, 'Factor1'] == vec_fac1[i] &
                          df_groups[,  'Factor2'] == vec_fac2[j], 'col'] = vec_col[i]
            df_groups[df_groups[, 'Factor1'] == vec_fac1[i] &
                          df_groups[,  'Factor2'] == vec_fac2[j], 'pch'] = vec_pch[j]
        }
    }
    
    return(df_groups)
}

setGroups = function(factor1, factor2 = NULL, row_names = NULL, order_fac1 = NULL, order_fac2 = NULL, order_col = NULL, order_pch = NULL){
    
    if(!is.null(factor1) & is.null(factor2)){
        return(setGroups1(factor1, row_names, order_fac1, order_col, order_pch))
    }
    
    if(!is.null(factor1) & !is.null(factor2)){
        return(setGroups2(factor1, factor2, row_names, order_fac1, order_fac2, order_col, order_pch))
    }
    
    stop(simpleError("Wrong input! please check again."))
}

# Get unique groups
getGroups1 = function(df_groups, order_fac1){
    
    vec_fac1 = sort(unique(df_groups[, 'Factor1']))
    n_fac1 = length(vec_fac1)
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    vec_col = c()
    vec_pch = c()
    for(each in vec_fac1){
        vec_col = c(vec_col, df_groups[df_groups[, 'Factor1'] == each, 'col'][1])
        vec_pch = c(vec_pch, df_groups[df_groups[, 'Factor1'] == each, 'pch'][1])
    }
    
    return(data.frame(Factor1 = vec_fac1, col = vec_col, pch = vec_pch))
}

getGroups2 = function(df_groups, order_fac1, order_fac2){
    
    vec_fac1 = sort(unique(df_groups[, 'Factor1']))
    n_fac1 = length(vec_fac1)
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    vec_fac2 = sort(unique(df_groups[, 'Factor2']))
    n_fac2 = length(vec_fac2)
    if(!is.null(order_fac2)){
        if(setequal(order_fac2, vec_fac2)){
            vec_fac2 = order_fac2
        }
        else{
            stop(simpleError("The unique elements in [factor2] are incompitable with [order_fac2]"))
        }
    }
    
    vec_fac1_out = c()
    vec_fac2_out = c()
    vec_facs_out = c()
    vec_col = c()
    vec_pch = c()
    for(fac1 in vec_fac1){
        for(fac2 in intersect(vec_fac2, unique(df_groups[df_groups[, 'Factor1'] == fac1, 'Factor2']))){
            vec_fac1_out = c(vec_fac1_out, fac1)
            vec_fac2_out = c(vec_fac2_out, fac2)
            vec_facs_out = c(vec_facs_out, paste(fac1, fac2, sep = '_'))
            vec_col = c(vec_col, unique(df_groups[df_groups[, 'Factor1'] == fac1 &
                                                      df_groups[, 'Factor2'] == fac2, 'col']))
            vec_pch = c(vec_pch, unique(df_groups[df_groups[, 'Factor1'] == fac1 &
                                                      df_groups[, 'Factor2'] == fac2, 'pch']))
        }
    }
    
    return(data.frame(Factor1 = vec_fac1_out, Factor2 = vec_fac2_out, Factors = vec_facs_out, col = vec_col, pch = vec_pch))
}

getGroups = function(df_groups, order_fac1 = NULL, order_fac2 = NULL){
    
    if('Factor2' %in% colnames(df_groups)) {
        return(getGroups2(df_groups, order_fac1, order_fac2))
    }
    else{
        return(getGroups1(df_groups, order_fac1))
    }
}

# Plot stacked bar
plotStackedBar0 = function(df_data,
                           main = '', ylab = '', vec_fac1_axis = NULL,
                           width = 1, space = 0.2, text_cex = 1.5, width_legend = 4.0, font_legend = 1, las = 2,
                           vec_col = NULL, n_column = 1){
    
    if(!is.data.frame(df_data) & !is.matrix(df_data)){
        stop(simpleError("ERROR: Wrong input, [df_data] should be 'data.frame' or 'matrix'!"))
    }
    
    vec_indices = colnames(df_data)
    n_indices = length(vec_indices)
    
    vec_bars = row.names(df_data)
    n_bars = length(vec_bars)
    
    # Asign color
    if(is.null(vec_col)){
        if(n_indices <= n_palette_ss_global){
            vec_col = palette_ss_global[1: n_indices]
        }
        else{
            quotient = floor(n_indices / n_palette_ss_global)
            remainder = n_indices - quotient * n_palette_ss_global
            vec_col = c(rep(palette_ss_global, quotient), palette_ss_global[1 : remainder])
        }
    }
    
    # Plot stacked bar
    bar = barplot(t(as.matrix(df_data)), width = width, space = space, xaxt = 'n', col = vec_col, cex.axis = 0.80 * text_cex, las = 2)
    title(main = main, cex.main = 1.20 * text_cex, ylab = ylab, cex.lab = text_cex, font.lab = 2)
    legend(x = (n_bars + 0.2) * (width + space), y = 0, legend = rev(vec_indices), fill = rev(vec_col), bty = 'n',
           text.width = width_legend, text.font = font_legend,
           cex = 0.80 * text_cex, xpd = T, xjust = 0, yjust = 0, ncol = n_column)
    
    # Draw axis
    if(is.null(vec_fac1_axis)){
        vec_fac1_axis = vec_bars
    }
    pos_fac1_axis = bar
    axis(side = 1, at = pos_fac1_axis, labels = vec_fac1_axis, cex.axis = 0.60 * text_cex, tick = F, las = las, line = -1)
}

plotStackedBar1 = function(df_data, factor1,
                           order_fac1 = NULL,
                           main = '', ylab = '', vec_fac1_axis = NULL,
                           width = 1, space = 0.2, text_cex = 1.5, width_legend = 4.0, font_legend = 1,
                           vec_col = NULL, n_column = 1){
    
    if(!is.data.frame(df_data) & !is.matrix(df_data)){
        stop(simpleError("ERROR: Wrong input, [df_data] should be 'data.frame' or 'matrix'!"))
    }
    
    vec_indices = colnames(df_data)
    n_indices = length(vec_indices)
    
    df_groups = data.frame(Groups1 = factor1)
    
    # Extract the unique groups
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Create matrix that should be plotted
    df_barplot_raw = cbind(df_groups, df_data)
    vec_barplot = c()
    for(fac1 in vec_fac1){
        vec_barplot = c(vec_barplot, colMeans(df_barplot_raw[df_barplot_raw$Groups1 == fac1, vec_indices]))
    }
    mat_barplot = matrix(vec_barplot, nrow = n_indices)
    n_bars = ncol(mat_barplot)
    
    # Asign color
    if(is.null(vec_col)){
        if(n_indices <= n_palette_ss_global){
            vec_col = palette_ss_global[1: n_indices]
        }
        else{
            quotient = floor(n_indices / n_palette_ss_global)
            remainder = n_indices - quotient * n_palette_ss_global
            vec_col = c(rep(palette_ss_global, quotient), palette_ss_global[1 : remainder])
        }
    }
    
    # Plot stacked bar
    bar = barplot(mat_barplot, width = width, space = space, xaxt = 'n', col = vec_col, cex.axis = 0.80 * text_cex, las = 2)
    title(main = main, cex.main = 1.20 * text_cex, ylab = ylab, cex.lab = text_cex, font.lab = 2)
    legend(x = (n_bars + 0.2) * (width + space), y = 0, legend = rev(vec_indices), fill = rev(vec_col), bty = 'n',
           text.width = width_legend, text.font = font_legend,
           cex = 0.80 * text_cex, xpd = T, xjust = 0, yjust = 0, ncol = n_column)
    
    # Draw axis
    if(is.null(vec_fac1_axis)){
        vec_fac1_axis = vec_fac1
    }
    pos_fac1_axis = bar
    axis(side = 1, at = pos_fac1_axis, labels = vec_fac1_axis, cex.axis = 0.80 * text_cex, tick = F, line = -1)
}

plotStackedBar2 = function(df_data, factor1, factor2,
                           order_fac1 = NULL, order_fac2 = NULL,
                           main = '', ylab = '', vec_fac1_axis = NULL, vec_fac2_axis = NULL,
                           width = 1, space = 0.2, text_cex = 1.5, width_legend = 4.0, font_legend = 1,
                           vec_col = NULL, n_column = 1){
    
    if(!is.data.frame(df_data) & !is.matrix(df_data)){
        stop(simpleError("ERROR: Wrong input, [df_data] should be 'data.frame' or 'matrix'!"))
    }
    
    # Judge whether the length of [factor1] and [factor2] are equal
    if(length(factor1) != length(factor2)){
        stop(simpleError("Vectors [factor1] and [factor2] have different lengths"))
    }
    
    vec_indices = colnames(df_data)
    n_indices = length(vec_indices)
    
    df_groups = data.frame(Groups1 = factor1, Groups2 = factor2)
    
    # Extract the unique groups in factor1
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    # Judge whether the [order_fac1] is compatible with the default
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Extract the unique groups in factor2
    vec_fac2 = sort(unique(factor2))
    n_fac2 = length(vec_fac2)
    # Judge whether the [order_fac2] is compatible with the default
    if(!is.null(order_fac2)){
        if(setequal(order_fac2, vec_fac2)){
            vec_fac2 = order_fac2
        }
        else{
            stop(simpleError("The unique elements in [factor2] are incompitable with [order_fac2]"))
        }
    }
    
    df_barplot_raw = cbind(df_groups, df_data)
    
    # Create matrix that should be plotted
    vec_barplot = c()
    vec_fac1_pos_default = c()
    vec_fac2_axis_default = c()
    lst_fac2_pos_default = list()
    pos_fac1_start = space / 2
    interval = width + space
    for(fac1 in vec_fac1){
        n_fac2_temp = 0
        vec_fac2_pos_temp = c()
        vec_fac2_temp = intersect(vec_fac2, unique(df_barplot_raw[df_barplot_raw$Groups1 == fac1, 'Groups2']))
        for(fac2 in vec_fac2_temp){
            n_fac2_temp = n_fac2_temp + 1
            vec_barplot = c(vec_barplot, colMeans(df_barplot_raw[df_barplot_raw$Groups1 == fac1 &
                                                                     df_barplot_raw$Groups2 == fac2, vec_indices]))
            vec_fac2_axis_default = c(vec_fac2_axis_default, fac2)
            vec_fac2_pos_temp = c(vec_fac2_pos_temp, pos_fac1_start - (interval / 2) + interval * n_fac2_temp)
        }
        vec_fac1_pos_default = c(vec_fac1_pos_default, pos_fac1_start + (interval / 2) * n_fac2_temp)
        lst_fac2_pos_default[fac1] = list(vec_fac2_pos_temp)
        pos_fac1_start = pos_fac1_start + interval * n_fac2_temp
    }
    mat_barplot = matrix(vec_barplot, nrow = n_indices)
    n_bars = ncol(mat_barplot)
    
    # Assign color
    if(is.null(vec_col)){
        if(n_indices <= n_palette_ss_global){
            vec_col = palette_ss_global[1: n_indices]
        }
        else{
            quotient = floor(n_indices / n_palette_ss_global)
            remainder = n_indices - quotient * n_palette_ss_global
            vec_col = c(rep(palette_ss_global, quotient), palette_ss_global[1 : remainder])
        }
    }
    
    # Plot stacked bar
    bar = barplot(mat_barplot, width = width, space = space, xaxt = 'n', col = vec_col, cex.axis = 0.80 * text_cex, las = 2)
    title(main = main, cex.main = 1.20 * text_cex, ylab = ylab, cex.lab = text_cex, font.lab = 2)
    legend(x = (n_bars + 0.2) * (width + space), y = 0, legend = rev(vec_indices), fill = rev(vec_col), bty = 'n',
           text.width = width_legend, text.font = font_legend,
           cex = 0.80 * text_cex, xpd = T, xjust = 0, yjust = 0, ncol = n_column)
    
    # Draw labels of factor1
    if(is.null(vec_fac1_axis)){
        vec_fac1_axis = vec_fac1
    }
    axis(side = 1, at = vec_fac1_pos_default, labels = vec_fac1_axis, cex.axis = text_cex, cex.axis = 0.85 * text_cex, line = 1.00, tick = F)
    
    # Draw labels of factor2
    if(is.null(vec_fac2_axis)){
        vec_fac2_axis = vec_fac2_axis_default
    }
    axis(side = 1, at = bar, labels = vec_fac2_axis_default, lwd.ticks = 1.50, cex.axis = 0.75 * text_cex, line = -1.00, tick = F)
    
    # Draw axis
    for(fac1 in vec_fac1){
        axis(side = 1, at = lst_fac2_pos_default[fac1][[1]],
             labels = rep('', length(lst_fac2_pos_default[fac1][[1]])),
             lwd.ticks = 1.50,
             cex.axis = 0.80 * text_cex, line = 1.60, tick = T, tck = 0.01)
    }
}

plotStackedBar3 = function(df_data, factor1, factor2, factor3,
                           order_fac1 = NULL, order_fac2 = NULL, order_fac3 = NULL,
                           main = '', ylab = '', vec_fac1_axis = NULL, vec_fac2_axis = NULL, vec_fac3_axis = NULL,
                           width = 1, space = 0.2, text_cex = 1.5, width_legend = 4.0, font_legend = 1,
                           vec_col = NULL, n_column = 1){
    
    if(!is.data.frame(df_data) & !is.matrix(df_data)){
        stop(simpleError("ERROR: Wrong input, [df_data] should be 'data.frame' or 'matrix'!"))
    }
    
    # Judge whether the length of [factor1] and [factor2] are equal
    if(T %in% (length(factor1) != c(length(factor2), length(factor3)))){
        stop(simpleError("Vectors [factor1], [factor2] and [factor3] have different lengths"))
    }
    
    vec_indices = colnames(df_data)
    n_indices = length(vec_indices)
    
    df_groups = data.frame(Groups1 = factor1, Groups2 = factor2, Groups3 = factor3)
    
    # Extract the unique groups in factor1
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    # Judge whether the [order_fac1] is compatible with the default
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Extract the unique groups in factor2
    vec_fac2 = sort(unique(factor2))
    n_fac2 = length(vec_fac2)
    # Judge whether the [order_fac2] is compatible with the default
    if(!is.null(order_fac2)){
        if(setequal(order_fac2, vec_fac2)){
            vec_fac2 = order_fac2
        }
        else{
            stop(simpleError("The unique elements in [factor2] are incompitable with [order_fac2]"))
        }
    }
    
    # Extract the unique groups in factor3
    vec_fac3 = sort(unique(factor3))
    n_fac3 = length(vec_fac3)
    # Judge whether the [order_fac3] is compatible with the default
    if(!is.null(order_fac3)){
        if(setequal(order_fac3, vec_fac3)){
            vec_fac3 = order_fac3
        }
        else{
            stop(simpleError("The unique elements in [factor3] are incompitable with [order_fac3]"))
        }
    }
    
    df_barplot_raw = cbind(df_groups, df_data)
    
    vec_barplot = c()
    vec_fac1_pos_default = c()
    vec_fac2_axis_default = c()
    lst_fac2_pos_default = list()
    vec_fac3_axis_default = c()
    lst_fac3_pos_default = list()
    
    interval = width + space
    pos_ptr = -width / 2
    for(fac1 in vec_fac1){
        n_fac2_temp = 0
        vec_fac2_pos_temp = c()
        vec_fac2_temp = intersect(vec_fac2, unique(df_barplot_raw[df_barplot_raw$Groups1 == fac1, 'Groups2']))
        for(fac2 in vec_fac2_temp){
            vec_fac3_pos_temp = c()
            vec_fac3_temp = intersect(vec_fac3, unique(df_barplot_raw[df_barplot_raw$Groups1 == fac1 &
                                                                          df_barplot_raw$Groups2 == fac2, 'Groups3']))
            for(fac3 in vec_fac3_temp){
                vec_barplot = c(vec_barplot, colMeans(df_barplot_raw[df_barplot_raw$Groups1 == fac1 &
                                                                         df_barplot_raw$Groups2 == fac2 &
                                                                         df_barplot_raw$Groups3 == fac3, vec_indices]))
                pos_ptr = pos_ptr + interval
                pos_fac3_temp = pos_ptr
                names(pos_fac3_temp) = fac3
                vec_fac3_pos_temp = c(vec_fac3_pos_temp, pos_fac3_temp)
            }
            pos_fac2_temp = (vec_fac3_pos_temp[1] + tail(vec_fac3_pos_temp, 1)) / 2
            names(pos_fac2_temp) = fac2
            vec_fac2_pos_temp = c(vec_fac2_pos_temp, pos_fac2_temp)
            lst_fac3_pos_default[[fac1]][[fac2]] = vec_fac3_pos_temp
        }
        pos_fac1_temp = (vec_fac2_pos_temp[1] + tail(vec_fac2_pos_temp, 1)) / 2
        names(pos_fac1_temp) = fac1
        vec_fac1_pos_default = c(vec_fac1_pos_default, pos_fac1_temp)
        lst_fac2_pos_default[fac1] = list(vec_fac2_pos_temp)
    }
    mat_barplot = matrix(vec_barplot, nrow = n_indices)
    n_bars = ncol(mat_barplot)
    
    # Assign color
    if(is.null(vec_col)){
        if(n_indices <= n_palette_ss_global){
            vec_col = palette_ss_global[1: n_indices]
        }
        else{
            quotient = floor(n_indices / n_palette_ss_global)
            remainder = n_indices - quotient * n_palette_ss_global
            vec_col = c(rep(palette_ss_global, quotient), palette_ss_global[1 : remainder])
        }
    }
    
    # Plot stacked bar
    bar = barplot(mat_barplot, width = width, space = space, xaxt = 'n', col = vec_col, cex.axis = 0.80 * text_cex, las = 2)
    title(main = main, cex.main = 1.20 * text_cex, ylab = ylab, cex.lab = text_cex, font.lab = 2)
    legend(x = (n_bars + 0.2) * (width + space), y = 0, legend = rev(vec_indices), fill = rev(vec_col), bty = 'n',
           text.width = width_legend, text.font = font_legend,
           cex = 0.80 * text_cex, xpd = T, xjust = 0, yjust = 0, ncol = n_column)
    
    # Draw labels of factor1
    axis(side = 1, at = vec_fac1_pos_default, labels = names(vec_fac1_pos_default), cex.axis = 0.85 * text_cex, line = 3.00, tick = F, hadj = 0.75)
    for(fac1 in names(vec_fac1_pos_default)){
        # Draw labels and axis of factor2
        axis(side = 1, at = lst_fac2_pos_default[[fac1]], labels = names(lst_fac2_pos_default[[fac1]]),
             cex.axis = 0.80 * text_cex, line = 0.80, tick = F)
        axis(side = 1, at = lst_fac2_pos_default[[fac1]],
             labels = rep('', length(lst_fac2_pos_default[[fac1]])),
             cex.axis = 0.80 * text_cex, line = 3.50, tick = T, tck = 0.01)
        for(fac2 in names(lst_fac2_pos_default[[fac1]])){
            # Draw labels and axis of factor3
            axis(side = 1, at = lst_fac3_pos_default[[fac1]][[fac2]],
                 labels = names(lst_fac3_pos_default[[fac1]][[fac2]]),
                 cex.axis = 0.75 * text_cex, line = -1.00, tick = F)
            axis(side = 1, at = lst_fac3_pos_default[[fac1]][[fac2]],
                 labels = rep('', length(lst_fac3_pos_default[[fac1]][[fac2]])),
                 cex.axis = 0.75 * text_cex, line = 1.50, tick = T, tck = 0.01)
        }
    }
}

plotStackedBar = function(df_data, factor1 = NULL, factor2 = NULL, factor3 = NULL,
                          order_fac1 = NULL, order_fac2 = NULL, order_fac3 = NULL,
                          main = '', ylab = '', vec_fac1_axis = NULL, vec_fac2_axis = NULL, vec_fac3_axis = NULL,
                          width = 1, space = 0.2, text_cex = 1.5, width_legend = 4.0, font_legend = 1, las = 2,
                          vec_col = NULL, n_column = 1) {
    
    if(is.null(factor1) & is.null(factor2) & is.null(factor3)){
        return(plotStackedBar0(df_data,
                               main, ylab, vec_fac1_axis,
                               width, space, text_cex, width_legend, font_legend, las,
                               vec_col, n_column))
    }
    
    if(!is.null(factor1) & is.null(factor2) & is.null(factor3)){
        return(plotStackedBar1(df_data, factor1,
                               order_fac1,
                               main, ylab, vec_fac1_axis,
                               width, space, text_cex, width_legend, font_legend,
                               vec_col, n_column))
    }
    
    if(!is.null(factor1) & !is.null(factor2) & is.null(factor3)){
        return(plotStackedBar2(df_data, factor1, factor2,
                               order_fac1, order_fac2,
                               main, ylab, vec_fac1_axis, vec_fac2_axis,
                               width, space, text_cex, width_legend, font_legend,
                               vec_col, n_column))
    }
    
    if(!is.null(factor1) & !is.null(factor2) & !is.null(factor3)){
        return(plotStackedBar3(df_data, factor1, factor2, factor3,
                               order_fac1, order_fac2, order_fac3,
                               main, ylab, vec_fac1_axis, vec_fac2_axis, vec_fac3_axis,
                               width, space, text_cex, width_legend, font_legend,
                               vec_col, n_column))
    }
    
    stop(simpleError("Wrong input! please check again."))
}

# Compare groups and get p-value
compGroups = function(index, factor){
    
    # Judge whether the length of [index] and [factor] are equal
    if(length(index) != length(factor)){
        stop(simpleError("Vectors [index] and [factor] have different lengths"))
    }
    
    # Get the number of groups
    groups = as.vector(unique(factor))
    n_groups = length(groups)
    
    # When the number of groups is less than 2, raise 'Error'
    if(n_groups < 2){
        stop(simpleError("Vector [factor] contain less than 2 groups"))
    }
    # When the number of groups is equal to 2, use 't-test' or 'Wilcoxon-test'
    else if(n_groups == 2){
        index1 = index[factor == groups[1]]
        if(!(F %in% (index1 == index1[1]))){
            print(index)
            warning('All values in group 1 are the same!')
            index1 = index1 * runif(length(index1), 0.95, 1.05)
        }
        
        index2 = index[factor == groups[2]]
        if(!(F %in% (index2 == index2[1]))){
            warning('All values in group 2 are the same!')
            index2 = index2 * runif(length(index2), 0.95, 1.05)
        }
        
        res_shapiro1 = shapiro.test(index)
        res_shapiro2 = shapiro.test(index)
        
        # When the data don't conforms to a normal distribution, use Wilcoxon-test
        if(T %in% (c(res_shapiro1$p.value, res_shapiro2$p.value) < 0.1)){
            index_tTest = wilcox.test(index1, index2, alternative = 'greater', exact = F)
        }
        # When the data conforms to a normal distribution, use t-test
        else{
            index_tTest = t.test(index1, index2, alternative = 'greater')
        }
        
        pValue = index_tTest$p.value
        
        if(is.na(pValue)){
            warning("p.value is 'NULL'! Return 0.5 instead.")
            return(0.5)
        }
        
        if(pValue > 0.5){
            return(1 - pValue)
        }
        
        return(pValue)
    }
    # When the number of groups is greater than 2, use 'ANOVA'
    else {
        
        is_norm = T
        for(group in groups){
            res_shapiro = shapiro.test(index[factor == group])
            if(res_shapiro$p.value < 0.10){
                is_norm = F
            }
        }
        
        # When the data conforms to a normal distribution, use ANOVA
        if(is_norm){
            index_lm = lm(formula = index ~ factor)
            index_anova = anova(index_lm)
            pValue = index_anova$`Pr(>F)`[1]
        }
        # When the data don't conforms to a normal distribution, use Kruskal-test
        else{
            res_kruskal = kruskal.test(index, factor)
            pValue = res_kruskal$p.value
        }
        
        return(pValue)
    }
}

# Get asterisk marks from p-value
calcAsterisk = function(pValue, threshold = 0.050){
    
    vec_asterisk = c()
    for(each in pValue){
        if(each < min(c(0.001, threshold))){
            vec_asterisk = c(vec_asterisk, '***')
        }
        else if(each < min(c(0.010, threshold))){
            vec_asterisk = c(vec_asterisk, '**')
        }
        else if(each < min(c(0.050, threshold))){
            vec_asterisk = c(vec_asterisk, '*')
        }
        else if(each < min(c(0.100, threshold))){
            vec_asterisk = c(vec_asterisk, '·')
        }
        else{
            vec_asterisk = c(vec_asterisk, '')
        }
    }
    return(vec_asterisk)
}

# Plot dots with error bars
plotDot1 = function(index, factor1,
                    order_fac1 = NULL,
                    main = '', xlab = '', ylab = '', if_asterisk = T, if_abline = T,
                    order_col = NULL, order_pch = NULL, col_edge = rgb(0, 0, 0, 1),
                    text_cex = 1, cex_mark = 1, conf = 0.95, top_margin = NULL, bottom_margin = NULL,
                    if_legend = T, pos_legend = 'topright', order_legend = NULL, n_column_legend = NULL){
    
    # Judge whether the length of [index] and [factor1] are equal to each other
    if(length(index) != length(factor1)){
        stop(simpleError("Vectors [index] and [factor1] have different lengths"))
    }
    
    # Merge index, factor1 and factor2 into 1 data.frame
    df = data.frame(Index = index, Factor1 = factor1)
    
    # Set point shapes and colors for factor1 and factor2
    df_groups = setGroups(factor1 = factor1,
                          order_fac1 = order_fac1,
                          order_pch = order_pch, order_col = order_col)
    
    # Extract the unique groups in factor1
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    # Judge whether the [order_fac1] is compatible with the default
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Set the maximum of group number
    n_fac_max = n_fac1
    
    # Calculate p.value at level 1
    pValue_total = compGroups(index, factor1)
    
    # Assigned position to each point and each group in factor1
    vec_pos_points = c()
    vec_pos_axes1 = c()
    vec_pos_lines = c()
    
    vec_mean = c()
    vec_stdErr = c()
    vec_col = c()
    vec_pch = c()
    
    for(n in c(1 : n_fac1)){
        vec_f1 = df[df[, 'Factor1'] == vec_fac1[n], 'Index']
        
        vec_col = c(vec_col, df_groups[df_groups[, 'Factor1'] == vec_fac1[n], 'col'][1])
        vec_pch = c(vec_pch, df_groups[df_groups[, 'Factor1'] == vec_fac1[n], 'pch'][1])
        
        index_mean = mean(vec_f1)
        index_stdErr = (sd(vec_f1) / sqrt(length(vec_f1))) * qt(conf / 2 + 0.5, length(vec_f1) - 1)
        #index_std = sd(vec_f1)
        #index_stdErr = std.error(vec_f1)
        vec_mean = c(vec_mean, index_mean)
        vec_stdErr = c(vec_stdErr, index_stdErr)
    }
    
    vec_pos_axes1 = c(1 : n_fac_max)
    index_min = min(vec_mean - vec_stdErr)
    index_max = max(vec_mean + vec_stdErr)
    yDelta = index_max - index_min
    yMax = index_max + 0.35 * yDelta
    yMin = index_min - 0.15 * yDelta
    vec_pos_lines = seq(1.5, n_fac_max - 0.5, length.out = n_fac_max - 1)
    vec_pos_points = vec_pos_axes1
    
    if(if_legend){
        if(pos_legend %in% c('topleft', 'top', 'topright')){
            if(is.null(top_margin)){
                top_margin = 0.50
            }
        }
        else{
            warning("Inappropriate position of legend, it may overlap with other graphics or text!")
        }
    }
    
    if(is.null(top_margin)){
        top_margin = 0
    }
    
    if(is.null(bottom_margin)){
        bottom_margin = 0
    }
    
    yTop = index_max + (top_margin + 0.35) * yDelta
    yBot = index_min - (bottom_margin + 0.15) * yDelta
    
    plot(vec_pos_points, vec_mean, type = 'n', xaxt = 'n', yaxt = 'n',
         ylim = c(yBot, yTop), xlab = xlab, ylab = ylab,
         cex.lab = 2.00 * text_cex, xlim = c(0.5, n_fac_max + 0.5), main = main, cex.main = 2.50 * text_cex)
    
    if(if_abline){
        abline(v = vec_pos_lines, col = 'grey', lwd = 2.50, lty = 10)
    }
    
    arrows(vec_pos_points, vec_mean - vec_stdErr, vec_pos_points, vec_mean + vec_stdErr,
           length = 0.03 * cex_mark, angle = 90, code = 3, lwd = 1.50 * cex_mark)
    
    points(vec_pos_points, vec_mean, pch = vec_pch, cex = 3.00 * cex_mark, col = col_edge, bg = vec_col, lwd = cex_mark)
    
    axis(side = 1, at = vec_pos_axes1, labels = rep('', n_fac1), lwd.ticks = 2, cex.axis = 2.00 * text_cex,
         tick = T, tck = -0.02)
    axis(side = 1, at = vec_pos_axes1, labels = vec_fac1, lwd.ticks = 2, cex.axis = 2.00 * text_cex,
         line = 0.15, tick = F)
    axis(side = 2, cex.axis = 1.50 * text_cex, las = 1)
    
    df_groups_unique = getGroups(df_groups, order_fac1 = order_fac1)
    
    if(if_legend){
        vec_facs = df_groups_unique[, 'Factor1']
        n_facs = length(vec_facs)
        vec_col_legend = df_groups_unique[, 'col']
        vec_pch_legend = df_groups_unique[, 'pch']
        # Judge whether the [order_legend] is compatible with the default
        if(!is.null(order_legend)){
            if(setequal(order_legend, vec_facs)){
                vec_facs = order_legend
                vec_col_legend = c()
                vec_pch_legend = c()
                for(each in vec_facs){
                    vec_col_legend = c(vec_col_legend,
                                       df_groups_unique[df_groups_unique[, 'Factor1'] == each, 'col'])
                    vec_pch_legend = c(vec_pch_legend,
                                       df_groups_unique[df_groups_unique[, 'Factor1'] == each, 'pch'])
                }
            }
            else{
                warning(paste(c('[Factor1]:', df_groups_unique[, 'Factor1']), collapse = '  '))
                warning(paste(c('[order_legend]:', order_legend), collapse = '  '))
                stop(simpleError("The unique elements in [Factor1] are incompitable with [order_legend]"))
            }
        }
        
        if(is.null(n_column_legend)){
            n_column_legend = 1
        }
        
        legend(pos_legend, legend = df_groups_unique[, 'Factor1'],
               pch = vec_pch_legend, col = col_edge, pt.bg = vec_col_legend,
               cex = 1.50 * text_cex, pt.cex = 2.00 * cex_mark, text.font = 1, bty = 'n', ncol = n_column_legend)
    }
    
    # Add p.value or significance
    conf1 = 0.05
    if(pValue_total < conf1 & if_asterisk){
        arrows(vec_pos_axes1[1], yMax - 0.080 * yDelta,
               vec_pos_axes1[n_fac1], yMax - 0.080 * yDelta,
               length = 0.04, angle = 90, code = 3, lwd = 1.50)
        text(x = c((vec_pos_axes1[1] + vec_pos_axes1[n_fac1]) / 2), y = yMax - 0.020 * yDelta,
             labels = calcAsterisk(pValue_total), cex = 3.00)
    }
}

plotDot2 = function(index, factor1, factor2,
                    order_fac1 = NULL, order_fac2 = NULL,
                    main = '', xlab = '', ylab = '', if_asterisk = T, if_abline = T,
                    order_col = NULL, order_pch = NULL, col_edge = rgb(0, 0, 0, 1),
                    text_cex = 1, cex_mark = 1, conf = 0.95, top_margin = NULL, bottom_margin = NULL,
                    if_legend = T, pos_legend = 'topright', order_legend = NULL, n_column_legend = NULL){
    
    # Judge whether the length of [index], [factor1] and [factor2] are equal to each other
    if(T %in% (length(index) != c(length(factor1), length(factor2)))){
        stop(simpleError("Vectors [index], [factor1] and [factor2] have different lengths"))
    }
    
    # Merge index, factor1 and factor2 into 1 data.frame
    df = data.frame(Index = index, Factor1 = factor1, Factor2 = factor2)
    
    # Set point shapes and colors for factor1 and factor2
    df_groups = setGroups(factor1 = factor2, factor2 = factor1,
                          order_fac1 = order_fac2, order_fac2 = order_fac1,
                          order_pch = order_pch, order_col = order_col)
    
    # Extract the unique groups in factor1
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    # Judge whether the [order_fac1] is compatible with the default
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Extract the unique groups in factor2
    vec_fac2 = sort(unique(factor2))
    n_fac2 = length(vec_fac2)
    # Judge whether the [order_fac2] is compatible with the default
    if(!is.null(order_fac2)){
        if(setequal(order_fac2, vec_fac2)){
            vec_fac2 = order_fac2
        }
        else{
            stop(simpleError("The unique elements in [factor2] are incompitable with [order_fac2]"))
        }
    }
    
    # Set the maximum of group number
    n_fac_max = n_fac1 * n_fac2
    
    # Calculate p.value at level 1
    pValue_total = compGroups(index, factor1)
    
    # Assigned position to each point and each group in factor1
    vec_pos_points = c()
    vec_pos_axes1 = c()
    vec_pos_axes2 = c()
    vec_col_bar = c()
    vec_pos_lines = c()
    vec_pos_arrowL2 = c()
    vec_pos_arrowR2 = c()
    vec_group2_axis = c()
    start2 = 0
    
    vec_mean = c()
    vec_stdErr = c()
    vec_ms_max2 = c()
    vec_col = c()
    vec_pch = c()
    
    vec_pValue2 = c()
    
    for(n in c(1 : n_fac1)){
        vec_f1 = df[df[, 'Factor1'] == vec_fac1[n], 'Index']
        vec_group2 = df[df[, 'Factor1'] == vec_fac1[n], 'Factor2']
        vec_fac2_temp = intersect(vec_fac2, unique(vec_group2))
        n_fac2_temp = length(vec_fac2_temp)
        
        # Append the axis labels for axis level 2
        vec_group2_axis = c(vec_group2_axis, vec_fac2_temp)
        
        # Calculate p.value at level 2
        vec_pValue2 = c(vec_pValue2, compGroups(vec_f1, vec_group2))
        
        vec_ms_max_temp = c()
        vec_mean_se = c()
        for(m in c(1 : n_fac2_temp)){
            vec_f1f2 = df[df[, 'Factor1'] == vec_fac1[n] & df[, 'Factor2'] == vec_fac2_temp[m], 'Index']
            
            vec_col = c(vec_col, df_groups[df_groups[, 'Factor1'] == vec_fac2_temp[m], 'col'][1])
            vec_pch = c(vec_pch, df_groups[df_groups[, 'Factor2'] == vec_fac1[n], 'pch'][1])
            
            index_mean = mean(vec_f1f2)
            index_stdErr = (sd(vec_f1f2) / sqrt(length(vec_f1f2))) * qt(conf / 2 + 0.5, length(vec_f1f2) - 1)
            #index_std = sd(vec_f1f2)
            #index_stdErr = std.error(vec_f1f2)
            vec_mean = c(vec_mean, index_mean)
            vec_stdErr = c(vec_stdErr, index_stdErr)
            vec_mean_se = c(vec_mean_se, index_mean + index_stdErr)
            vec_ms_max_temp = c(vec_ms_max_temp, max(vec_mean_se))
        }
        vec_ms_max2 = c(vec_ms_max2, max(vec_ms_max_temp))
        
        vec_pos_temp2 = seq(from = start2 + n_fac_max / (n_fac1 * (n_fac2_temp + 1)),
                            to = start2 + n_fac2_temp * n_fac_max / (n_fac1 * (n_fac2_temp + 1)), length.out = n_fac2_temp)
        vec_pos_points = c(vec_pos_points, vec_pos_temp2)
        
        vec_pos_arrowL2 = c(vec_pos_arrowL2, vec_pos_temp2[1])
        vec_pos_arrowR2 = c(vec_pos_arrowR2, vec_pos_temp2[n_fac2_temp])
        vec_pos_axes1 = c(vec_pos_axes1, start2 + n_fac_max / (2 * n_fac1))
        vec_pos_lines = c(vec_pos_lines, start2)
        start2 = start2 + n_fac_max / n_fac1
    }
    
    index_min = min(vec_mean - vec_stdErr)
    index_max = max(vec_mean + vec_stdErr)
    yDelta = index_max - index_min
    yMax = index_max + 0.35 * yDelta
    yMin = index_min - 0.15 * yDelta
    vec_pos_lines = vec_pos_lines[-1]
    
    if(if_legend){
        if(pos_legend %in% c('topleft', 'top', 'topright')){
            if(is.null(top_margin)){
                top_margin = 0.50
            }
        }
        else{
            warning("Inappropriate position of legend, it may overlap with other graphics or text!")
        }
    }
    
    if(is.null(top_margin)){
        top_margin = 0
    }
    
    if(is.null(bottom_margin)){
        bottom_margin = 0
    }
    
    yTop = index_max + (top_margin + 0.35) * yDelta
    yBot = index_min - (bottom_margin + 0.15) * yDelta
    
    plot(vec_pos_points, vec_mean, type = 'n', xaxt = 'n', yaxt = 'n',
         ylim = c(yBot, yTop), xlab = xlab, ylab = ylab,
         cex.lab = 2.00 * text_cex, xlim = c(0, n_fac_max), main = main, cex.main = 2.50 * text_cex)
    
    if(if_abline){
        abline(v = vec_pos_lines, col = 'grey', lwd = 2.50, lty = 10)
    }
    
    arrows(vec_pos_points, vec_mean - vec_stdErr, vec_pos_points, vec_mean + vec_stdErr,
           length = 0.03 * cex_mark, angle = 90, code = 3, lwd = 1.50 * cex_mark)
    
    points(vec_pos_points, vec_mean, pch = vec_pch, cex = 3.00 * cex_mark, col = col_edge, bg = vec_col, lwd = cex_mark)
    
    axis(side = 1, at = vec_pos_axes1, labels = rep('', n_fac1), lwd.ticks = 2, cex.axis = 2.00 * text_cex,
         tick = T, tck = -0.02)
    axis(side = 1, at = vec_pos_axes1, labels = vec_fac1, lwd.ticks = 2, cex.axis = 2.00 * text_cex,
         line = 0.15, tick = F)
    axis(side = 2, cex.axis = 1.50 * text_cex, las = 1)
    
    df_groups_unique = getGroups(df_groups, order_fac1 = order_fac2, order_fac2 = order_fac1)
    
    if(if_legend){
        vec_facs = df_groups_unique[, 'Factors']
        n_facs = length(vec_facs)
        vec_col_legend = df_groups_unique[, 'col']
        vec_pch_legend = df_groups_unique[, 'pch']
        # Judge whether the [order_legend] is compatible with the default
        if(!is.null(order_legend)){
            if(setequal(order_legend, vec_facs)){
                vec_facs = order_legend
                vec_col_legend = c()
                vec_pch_legend = c()
                for(each in vec_facs){
                    vec_col_legend = c(vec_col_legend,
                                       df_groups_unique[df_groups_unique[, 'Factors'] == each, 'col'])
                    vec_pch_legend = c(vec_pch_legend,
                                       df_groups_unique[df_groups_unique[, 'Factors'] == each, 'pch'])
                }
            }
            else{
                warning(paste(c('[Factors]:', df_groups_unique[, 'Factors']), collapse = '  '))
                warning(paste(c('[order_legend]:', order_legend), collapse = '  '))
                stop(simpleError("The unique elements in [Factors] are incompitable with [order_legend]"))
            }
        }
        
        if(is.null(n_column_legend)){
            n_column_legend = n_fac1
        }
        
        legend(pos_legend, legend = df_groups_unique[, 'Factors'],
               pch = vec_pch_legend, col = col_edge, pt.bg = vec_col_legend,
               cex = 1.50 * text_cex, pt.cex = 2.00 * cex_mark, text.font = 1, bty = 'n', ncol = n_column_legend)
    }
    
    # Add p.value or significance
    conf1 = 0.05
    if(pValue_total < conf1 & if_asterisk){
        arrows(vec_pos_axes1[1], yMax - 0.080 * yDelta,
               vec_pos_axes1[n_fac1], yMax - 0.080 * yDelta,
               length = 0.04, angle = 90, code = 3, lwd = 1.50)
        text(x = c((vec_pos_axes1[1] + vec_pos_axes1[n_fac1]) / 2), y = yMax - 0.020 * yDelta,
             labels = calcAsterisk(pValue_total), cex = 3.00)
    }
    
    conf2 = 0.05
    if((TRUE %in% vec_pValue2 < conf2) & if_asterisk){
        arrows(vec_pos_arrowL2[which(vec_pValue2 < conf2)], vec_ms_max2[which(vec_pValue2 < conf2)] + 0.110 * yDelta,
               vec_pos_arrowR2[which(vec_pValue2 < conf2)], vec_ms_max2[which(vec_pValue2 < conf2)] + 0.110 * yDelta,
               length = 0.03, angle = 90, code = 3, lwd = 1.50)
        for(each in vec_pValue2[which(vec_pValue2 < conf2)]){
            text(x = vec_pos_axes1[which(vec_pValue2 == each)], y = vec_ms_max2[which(vec_pValue2 == each)] + 0.160 * yDelta,
                 labels = calcAsterisk(each), cex = 2.50)
        }
    }
}

plotDot3 = function(index, factor1, factor2, factor3,
                    order_fac1 = NULL, order_fac2 = NULL, order_fac3 = NULL,
                    main = '', xlab = '', ylab = '', if_asterisk = T, if_abline = T,
                    order_col = NULL, order_pch = NULL, col_edge = rgb(0, 0, 0, 1),
                    text_cex = 1, cex_mark = 1, conf = 0.95, top_margin = NULL, bottom_margin = NULL,
                    if_legend = T, pos_legend = 'topright', order_legend = NULL, n_column_legend = NULL){
    
    # Judge whether the length of [index], [factor1], [factor2] and [factor3] are equal to each other
    if(T %in% (length(index) != c(length(factor1), length(factor2), length(factor3)))){
        stop(simpleError("Vectors [index], [factor1], [factor2] and [factor3] have different lengths"))
    }
    
    # Merge index, factor1, factor2 and factor3 into 1 data.frame
    df = data.frame(Index = index, Factor1 = factor1, Factor2 = factor2, Factor3 = factor3)
    
    # Set point shapes and colors for factor2 and factor3
    df_groups = setGroups(factor1 = factor3, factor2 = factor2,
                          order_fac1 = order_fac3, order_fac2 = order_fac2,
                          order_pch = order_pch, order_col = order_col)
    
    # Extract the unique groups in factor1
    vec_fac1 = sort(unique(factor1))
    n_fac1 = length(vec_fac1)
    # Judge whether the [order_fac1] is compatible with the default
    if(!is.null(order_fac1)){
        if(setequal(order_fac1, vec_fac1)){
            vec_fac1 = order_fac1
        }
        else{
            stop(simpleError("The unique elements in [factor1] are incompitable with [order_fac1]"))
        }
    }
    
    # Extract the unique groups in factor2
    vec_fac2 = sort(unique(factor2))
    n_fac2 = length(vec_fac2)
    # Judge whether the [order_fac2] is compatible with the default
    if(!is.null(order_fac2)){
        if(setequal(order_fac2, vec_fac2)){
            vec_fac2 = order_fac2
        }
        else{
            stop(simpleError("The unique elements in [factor2] are incompitable with [order_fac2]"))
        }
    }
    
    # Extract the unique groups in factor3
    vec_fac3 = sort(unique(factor3))
    n_fac3 = length(vec_fac3)
    # Judge whether the [order_fac3] is compatible with the default
    if(!is.null(order_fac3)){
        if(setequal(order_fac3, vec_fac3)){
            vec_fac3 = order_fac3
        }
        else{
            stop(simpleError("The unique elements in [factor3] are incompitable with [order_fac3]"))
        }
    }
    
    # Set the maximum of group number
    n_fac_max = n_fac1 * n_fac2 * n_fac3
    
    # Calculate p.value at level 1
    pValue_total = compGroups(index, factor1)
    
    # Assigned position to each point and each group in factor1
    vec_pos_points = c()
    vec_pos_axes1 = c()
    vec_pos_axes2 = c()
    vec_col_bar = c()
    vec_pos_lines = c()
    vec_pos_arrowL2 = c()
    vec_pos_arrowR2 = c()
    vec_pos_arrowL3 = c()
    vec_pos_arrowR3 = c()
    vec_group2_axis = c()
    start2 = 0
    start3 = 0
    
    vec_mean = c()
    vec_stdErr = c()
    vec_ms_max3 = c()
    vec_ms_max2 = c()
    vec_col = c()
    vec_pch = c()
    
    vec_pValue2 = c()
    vec_pValue3 = c()
    vec_FValue3 = c()
    vec_RSquare3 = c()
    
    for(n in c(1 : n_fac1)){
        vec_f1 = df[df[, 'Factor1'] == vec_fac1[n], 'Index']
        vec_group2 = df[df[, 'Factor1'] == vec_fac1[n], 'Factor2']
        vec_fac2_temp = intersect(vec_fac2, unique(vec_group2))
        n_fac2_temp = length(vec_fac2_temp)
        
        # Append the axis labels for axis level 2
        vec_group2_axis = c(vec_group2_axis, vec_fac2_temp)
        
        # Calculate p.value at level 2
        vec_pValue2 = c(vec_pValue2, compGroups(vec_f1, vec_group2))
        
        vec_ms_max_temp = c()
        start3 = start2 + 0.125 * n_fac_max / (n_fac1 * (n_fac2_temp + 0.25))
        for(m in c(1 : n_fac2_temp)){
            vec_f1f2 = df[df[, 'Factor1'] == vec_fac1[n] & df[, 'Factor2'] == vec_fac2_temp[m], 'Index']
            vec_group3 = df[df[, 'Factor1'] == vec_fac1[n] & df[, 'Factor2'] == vec_fac2_temp[m], 'Factor3']
            
            vec_fac3_temp = intersect(vec_fac3, unique(vec_group3))
            n_fac3_temp = length(vec_fac3_temp)
            
            # Calculate p.value at level 3
            vec_pValue3 = c(vec_pValue3, compGroups(vec_f1f2, vec_group3))
            
            # Calculate standard error and the maximum of each group in factor2
            vec_mean_se = c()
            for(l in c(1 : n_fac3_temp)){
                vec_f1f2f3 = df[df[, 'Factor1'] == vec_fac1[n] &
                                    df[, 'Factor2'] == vec_fac2_temp[m] &
                                    df[, 'Factor3'] == vec_fac3_temp[l], 'Index']
                
                vec_col = c(vec_col, df_groups[df_groups[, 'Factor1'] == vec_fac3_temp[l], 'col'][1])
                vec_pch = c(vec_pch, df_groups[df_groups[, 'Factor2'] == vec_fac2_temp[m], 'pch'][1])
                
                index_mean = mean(vec_f1f2f3)
                index_stdErr = (sd(vec_f1f2f3) / sqrt(length(vec_f1f2f3))) * qt(conf / 2 + 0.5, length(vec_f1f2f3) - 1)
                #index_std = sd(vec_f1f2f3)
                #index_stdErr = std.error(vec_f1f2f3)
                vec_mean = c(vec_mean, index_mean)
                vec_stdErr = c(vec_stdErr, index_stdErr)
                vec_mean_se = c(vec_mean_se, index_mean + index_stdErr)
            }
            
            vec_ms_max_temp = c(vec_ms_max_temp, max(vec_mean_se))
            
            vec_pos_temp3 = seq(from = start3 + 1.0 * n_fac_max / (n_fac1 * (n_fac2_temp + 0.25) * (n_fac3_temp + 1)),
                                to = start3 + (n_fac3_temp - 0.0) * n_fac_max / (n_fac1 * (n_fac2_temp + 0.25) * (n_fac3_temp + 1)),
                                length.out = n_fac3_temp)
            vec_pos_points = c(vec_pos_points, vec_pos_temp3)
            vec_pos_arrowL3 = c(vec_pos_arrowL3,
                                start3 + 0.75 * n_fac_max / (n_fac1 * (n_fac2_temp + 0.25) * (n_fac3_temp + 1)))
            vec_pos_arrowR3 = c(vec_pos_arrowR3,
                                start3 + (n_fac3_temp + 0.25) * n_fac_max / (n_fac1 * (n_fac2_temp + 0.25) * (n_fac3_temp + 1)))
            vec_pos_axes2 = c(vec_pos_axes2, start3 + n_fac_max / (2 * n_fac1 * (n_fac2_temp + 0.25)))
            start3 = start3 + n_fac_max / (n_fac1 * (n_fac2_temp + 0.25))
        }
        vec_ms_max3 = c(vec_ms_max3, vec_ms_max_temp)
        vec_ms_max2 = c(vec_ms_max2, max(vec_ms_max_temp))
        
        vec_pos_temp2 = seq(from = start2 + n_fac_max / (n_fac1 * (n_fac2_temp + 1)),
                            to = start2 + n_fac2_temp * n_fac_max / (n_fac1 * (n_fac2_temp + 1)), length.out = n_fac2_temp)
        vec_pos_arrowL2 = c(vec_pos_arrowL2, vec_pos_temp2[1])
        vec_pos_arrowR2 = c(vec_pos_arrowR2, vec_pos_temp2[n_fac2_temp])
        vec_pos_axes1 = c(vec_pos_axes1, start2 + n_fac_max / (2 * n_fac1))
        vec_pos_lines = c(vec_pos_lines, start2)
        start2 = start2 + n_fac_max / n_fac1
    }
    
    index_min = min(vec_mean - vec_stdErr)
    index_max = max(vec_mean + vec_stdErr)
    yDelta = index_max - index_min
    yMax = index_max + 0.35 * yDelta
    yMin = index_min - 0.15 * yDelta
    vec_pos_lines = vec_pos_lines[-1]
    
    if(if_legend){
        if(pos_legend %in% c('topleft', 'top', 'topright')){
            if(is.null(top_margin)){
                top_margin = 0.50
            }
        }
        else{
            warning("Inappropriate position of legend, it may overlap with other graphics or text!")
        }
    }
    
    if(is.null(top_margin)){
        top_margin = 0
    }
    
    if(is.null(bottom_margin)){
        bottom_margin = 0
    }
    
    yTop = index_max + (top_margin + 0.35) * yDelta
    yBot = index_min - (bottom_margin + 0.15) * yDelta
    
    plot(vec_pos_points, vec_mean, type = 'n', xaxt = 'n', yaxt = 'n',
         ylim = c(yBot, yTop), xlab = xlab, ylab = ylab,
         cex.lab = 2.00 * text_cex, xlim = c(0, n_fac_max), main = main, cex.main = 2.50 * text_cex)
    
    if(if_abline){
        abline(v = vec_pos_lines, col = 'grey', lwd = 2.50, lty = 10)
    }
    
    arrows(vec_pos_points, vec_mean - vec_stdErr, vec_pos_points, vec_mean + vec_stdErr,
           length = 0.03 * cex_mark, angle = 90, code = 3, lwd = 1.50 * cex_mark)
    
    points(vec_pos_points, vec_mean, pch = vec_pch, cex = 3.00 * cex_mark, col = col_edge, bg = vec_col, lwd = cex_mark)
    
    axis(side = 1, at = vec_pos_axes1, labels = rep('', n_fac1), lwd.ticks = 2, cex.axis = 2.00 * text_cex,
         tick = T, tck = -0.02)
    axis(side = 1, at = vec_pos_axes1, labels = vec_fac1, lwd.ticks = 2, cex.axis = 2.00 * text_cex,
         line = 0.15, tick = F)
    axis(side = 2, cex.axis = 1.50 * text_cex, las = 1)
    
    df_groups_unique = getGroups(df_groups, order_fac1 = order_fac3, order_fac2 = order_fac2)
    
    if(if_legend){
        vec_facs = df_groups_unique[, 'Factors']
        n_facs = length(vec_facs)
        vec_col_legend = df_groups_unique[, 'col']
        vec_pch_legend = df_groups_unique[, 'pch']
        # Judge whether the [order_legend] is compatible with the default
        if(!is.null(order_legend)){
            if(setequal(order_legend, vec_facs)){
                vec_facs = order_legend
                vec_col_legend = c()
                vec_pch_legend = c()
                for(each in vec_facs){
                    vec_col_legend = c(vec_col_legend,
                                       df_groups_unique[df_groups_unique[, 'Factors'] == each, 'col'])
                    vec_pch_legend = c(vec_pch_legend,
                                       df_groups_unique[df_groups_unique[, 'Factors'] == each, 'pch'])
                }
            }
            else{
                warning(paste(c('[Factors]:', df_groups_unique[, 'Factors']), collapse = '  '))
                warning(paste(c('[order_legend]:', order_legend), collapse = '  '))
                stop(simpleError("The unique elements in [Factors] are incompitable with [order_legend]"))
            }
        }
        
        if(is.null(n_column_legend)){
            n_column_legend = n_fac2
        }
        
        legend(pos_legend, legend = df_groups_unique[, 'Factors'],
               pch = vec_pch_legend, col = col_edge, pt.bg = vec_col_legend,
               cex = 1.50 * text_cex, pt.cex = 2.00 * cex_mark, text.font = 1, bty = 'n', ncol = n_column_legend)
    }
    
    # Add p.value or significance
    conf1 = 0.05
    if(pValue_total < conf1 & if_asterisk){
        arrows(vec_pos_axes1[1], yMax - 0.080 * yDelta,
               vec_pos_axes1[n_fac1], yMax - 0.080 * yDelta,
               length = 0.04, angle = 90, code = 3, lwd = 1.50)
        text(x = c((vec_pos_axes1[1] + vec_pos_axes1[n_fac1]) / 2), y = yMax - 0.020 * yDelta,
             labels = calcAsterisk(pValue_total), cex = 3.00)
    }
    
    conf2 = 0.05
    if((TRUE %in% vec_pValue2 < conf2) & if_asterisk){
        arrows(vec_pos_arrowL2[which(vec_pValue2 < conf2)], vec_ms_max2[which(vec_pValue2 < conf2)] + 0.110 * yDelta,
               vec_pos_arrowR2[which(vec_pValue2 < conf2)], vec_ms_max2[which(vec_pValue2 < conf2)] + 0.110 * yDelta,
               length = 0.03, angle = 90, code = 3, lwd = 1.50)
        for(each in vec_pValue2[which(vec_pValue2 < conf2)]){
            text(x = vec_pos_axes1[which(vec_pValue2 == each)], y = vec_ms_max2[which(vec_pValue2 == each)] + 0.160 * yDelta,
                 labels = calcAsterisk(each), cex = 2.50)
        }
    }
    
    conf3 = 0.05
    if((TRUE %in% vec_pValue3 < conf3) & if_asterisk){
        arrows(vec_pos_arrowL3[which(vec_pValue3 < conf3)], vec_ms_max3[which(vec_pValue3 < conf3)] + 0.040 * yDelta,
               vec_pos_arrowR3[which(vec_pValue3 < conf3)], vec_ms_max3[which(vec_pValue3 < conf3)] + 0.040 * yDelta,
               length = 0.02, angle = 90, code = 3, lwd = 1.50)
        for(each in vec_pValue3[which(vec_pValue3 < conf3)]){
            text(x = vec_pos_axes2[which(vec_pValue3 == each)], y = vec_ms_max3[which(vec_pValue3 == each)] + 0.080 * yDelta,
                 labels = calcAsterisk(each), cex = 2.00)
        }
    }
    
    axis(side = 1, at = vec_pos_axes2, labels = vec_group2_axis, cex.axis = 1.50 * text_cex,
         line = -2.50, tick = F, col.axis = rgb(0, 0, 0, 1))
}

plotDot = function(index, factor1, factor2 = NULL, factor3 = NULL,
                   order_fac1 = NULL, order_fac2 = NULL, order_fac3 = NULL,
                   main = '', xlab = '', ylab = '', if_asterisk = T, if_abline = T,
                   order_col = NULL, order_pch = NULL, col_edge = rgb(0, 0, 0, 1),
                   text_cex = 1, cex_mark = 1, conf = 0.95, top_margin = NULL, bottom_margin = NULL,
                   if_legend = T, pos_legend = 'topright', order_legend = NULL, n_column_legend = NULL){
    
    if(!is.null(factor1) & is.null(factor2) & is.null(factor3)){
        return(plotDot1(index, factor1,
                        order_fac1,
                        main, xlab, ylab, if_asterisk, if_abline,
                        order_col, order_pch, col_edge,
                        text_cex, cex_mark, conf, top_margin, bottom_margin,
                        if_legend, pos_legend, order_legend, n_column_legend))
    }
    
    if(!is.null(factor1) & !is.null(factor2) & is.null(factor3)){
        return(plotDot2(index, factor1, factor2,
                        order_fac1, order_fac2,
                        main, xlab, ylab, if_asterisk, if_abline,
                        order_col, order_pch, col_edge,
                        text_cex, cex_mark, conf, top_margin, bottom_margin,
                        if_legend, pos_legend, order_legend, n_column_legend))
    }
    
    if(!is.null(factor1) & !is.null(factor2) & !is.null(factor3)){
        return(plotDot3(index, factor1, factor2, factor3,
                        order_fac1, order_fac2, order_fac3,
                        main, xlab, ylab, if_asterisk, if_abline,
                        order_col, order_pch, col_edge,
                        text_cex, cex_mark, conf, top_margin, bottom_margin,
                        if_legend, pos_legend, order_legend, n_column_legend))
    }
    
    stop(simpleError("Wrong input! please check again."))
}

# Get cut-off value
cutOff = function(values, threshold = 0.001, digits = 3, method = 'less'){
    
    vec_cutoff = c()
    if(method == 'less'){
        fmt = paste("%.", digits, "f", sep = '')
        for(value in values){
            if(value < threshold){
                vec_cutoff = c(vec_cutoff, paste('<', sprintf(fmt, threshold)))
            }
            else{
                vec_cutoff = c(vec_cutoff, paste('=', sprintf(fmt, value)))
            }
        }
    }
    else if(method == 'greater'){
        options(digits = digits, scipen = digits - 7)
        for(value in values){
            if(value > threshold){
                vec_cutoff = c(vec_cutoff, paste('>', signif(threshold, digits)))
            }
            else{
                vec_cutoff = c(vec_cutoff, paste('=', signif(value, digits)))
            }
        }
        options(digits = 7, scipen = 0)
    }
    else{
        stop(simpleError("Wrong type of method, only 'less' and 'greater' are available"))
    }
    
    return(vec_cutoff)
}

# Plot NMDS
drawSurround = function(ord, factor, colour, type = 'oval', if_link = NULL,
                        kind = 'sd', conf = 0.95, lty = 1, lwd = 1, alpha = 0.30, choices = c(1, 2)){
    
    df_groups = data.frame(Factor = factor, col = colour)
    vec_fac = unique(df_groups[order(df_groups[, 'Factor']), 'Factor'])
    
    colour_unique = c()
    for(each in vec_fac){
        colour_unique = c(colour_unique, unique(df_groups[df_groups[, 'Factor'] == each, 'col']))
    }
    
    mat_rgb = col2rgb(colour_unique)
    r = mat_rgb['red', ] / 255
    g = mat_rgb['green', ] / 255
    b = mat_rgb['blue', ] / 255
    
    if(type == 'oval'){
        if(is.null(if_link)){
            if_link = F
        }
        
        if(if_link){
            ordibar(ord, df_groups[, 'Factor'],
                    col = rgb(0.20, 0.20, 0.20, 1.00),
                    lwd = lwd,
                    kind = kind, conf = conf, choices = choices)
        }
        
        ordiellipse(ord, df_groups[, 'Factor'],
                    col = colour_unique,
                    alpha = alpha, draw = 'polygon',
                    lty = lty, lwd = lwd, border = rgb(0.30, 0.30, 0.30, 1.00),
                    kind = kind, conf = conf, choices = choices)
    }
    else if(type == 'polygon'){
        if(is.null(if_link)){
            if_link = T
        }
        
        if(if_link){
            ordispider(ord, df_groups[, 'Factor'],
                       col = rgb(0.25, 0.25, 0.25, 0.30),
                       lwd = 1.50 * lwd,
                       spiders = 'centroid', choices = choices)
        }
        
        ordihull(ord, df_groups[, 'Factor'],
                 col = colour_unique,
                 draw = 'polygon', alpha = alpha,
                 lty = lty, lwd = 3.00 * lwd, border = rgb(r, g, b, 0.80),
                 choices = choices)
    }
    else{
        stop(simpleError("Wrong type of surrounding, only 'oval' and 'polygon' is available"))
    }
}

plotNMDS0 = function(df_data,
                     order_pch = 21, order_col = rgb(0.5, 0.5, 0.5),
                     distance = 'bray', k = 3, trymax = 1e4, choices = c(1, 2),
                     df_envfit = NULL, permutations = 1e4, p.max = 0.05,
                     text_cex = 1.00, main = 'Distribution of Data', xlab = NULL, ylab = NULL,
                     point_cex = 1.00, point_alpha = 1.00, point_edge_col = rgb(0, 0, 0, 1),
                     if_cross = T, x_margin = 0.00, y_margin = 0.00,
                     sur_type = 'oval', sur_if_link = NULL,
                     sur_kind = 'sd', sur_lty = NULL, sur_lwd = 1, sur_alpha = 0.30, sur_conf = 0.95,
                     if_cenpoint = F){
    
    # Use function 'metaMDS()' to reduce dimensionality
    nmds_res = metaMDS(df_data, distance = distance, k = k, trymax = trymax)
    
    # Calculate the range of x axis
    mds1_max = max(nmds_res$points[, 1])
    mds1_min = min(nmds_res$points[, 1])
    xDelta = mds1_max - mds1_min
    xMax = mds1_max + (0.10 + x_margin) * xDelta
    xMin = mds1_min - (0.10 + x_margin) * xDelta
    
    # Calculate the range of y axis
    mds2_max = max(nmds_res$points[, 2])
    mds2_min = min(nmds_res$points[, 2])
    yDelta = mds2_max - mds2_min
    yMax = mds2_max + (0.10 + y_margin) * yDelta
    yMin = mds2_min - (0.10 + y_margin) * yDelta
    
    # Set point shapes and colors
    vec_col = paste(order_col, str_sub(rgb(0, 0, 0, point_alpha), 8, 9), sep = '')
    vec_pch = order_pch
    
    # Plot the layout
    plot(nmds_res, display = 'sites', type = 'n', xlab = '',  ylab = '', las = TRUE, choices = choices,
         xlim = c(xMin, xMax), ylim = c(yMin, yMax), cex.axis = 0.75 * text_cex)
    
    # Judge whether to draw a center cross
    if(if_cross){
        abline(v = 0, h = 0, col = rgb(0.4, 0.4, 0.4), lwd = 2.00, lty = 4)
    }
    
    # Set the line type for surround border
    if(is.null(sur_lty)){
        sur_lty = 1
    }
    
    # Draw surround
    drawSurround(ord = nmds_res, factor = rep('', nrow(df_data)), colour = vec_col,
                 type = sur_type, if_link = sur_if_link, kind = sur_kind, conf = sur_conf,
                 lty = sur_lty, lwd = sur_lwd, alpha = sur_alpha)
    
    # Draw all points
    points(nmds_res$points[, c(choices[1], choices[2])],
           pch = vec_pch, col = point_edge_col, cex = 1.50 * point_cex, bg = vec_col)
    
    # Whether to draw the center points
    if(if_cenpoint){
        # Draw center points
        points(cenpoint_temp[1], cenpoint_temp[2], pch = vec_pch, col = rgb(0, 0, 0, 1),
               bg = str_sub(vec_col, 1, 7), cex = 2.25 * point_cex)
    }
    
    # Draw the arrows of environmental factors
    if(is.data.frame(df_envfit) | is.vector(df_envfit)){
        envfit_res = envfit(nmds_res, choices = choices, df_envfit, permutations = permutations)
        plot(envfit_res, choices = choices, p.max = p.max, col = 'black', cex = 1.00 * text_cex, font = 1)
    }
    
    # Set the sub-title that contains information of stress, p-value and R-square
    tt = substitute(plain('Stress')~s,
                    list(s = cutOff(nmds_res$stress)))
    
    # Draw main and sub title
    text(mean(c(xMax, xMin)), yMax + 0.20 * yDelta, labels = main, xpd = T, adj = 0.5, cex =  1.40 * text_cex, font = 2)
    text(mean(c(xMax, xMin)), yMax + 0.10 * yDelta, labels = tt, xpd = T, adj = 0.5, cex = 1.10 * text_cex)
    
    # Calculate the variation proportion of each axis
    vec_var_prop = round(proportions(apply(nmds_res$species, 2, var)) * 100, 2)
    
    # Write label for x axis
    if(is.null(xlab)){
        xlab = paste('NMDS ', choices[1], ' (', vec_var_prop[choices[1]], '% variation)', sep = '')
    }
    
    # Write label for y axis
    if(is.null(ylab)){
        ylab = paste('NMDS ', choices[2], ' (', vec_var_prop[choices[2]], '% variation)', sep = '')
    }
    
    # Draw x and y axis
    title(main = '', xlab = xlab, ylab = ylab, cex.lab = 1.20 * text_cex)
}

plotNMDS1 = function(df_data,
                     factor1,
                     order_fac1 = NULL,
                     order_pch = NULL, order_col = NULL,
                     label_fac1 = '',
                     distance = 'bray', k = 3, trymax = 1e4, choices = c(1, 2),
                     df_envfit = NULL, permutations = 1e4, p.max = 0.05,
                     text_cex = 1.00, main = 'Distribution of Data', xlab = NULL, ylab = NULL,
                     point_cex = 1.00, point_alpha = 1.00, point_edge_col = rgb(0, 0, 0, 1),
                     if_legend = T, pos_legend = 'topright',
                     if_cross = T, x_margin = 0.00, y_margin = 0.00,
                     sur_type = 'oval', sur_if_link = NULL,
                     sur_kind = 'sd', sur_lty = NULL, sur_lwd = 1, sur_alpha = 0.30, sur_conf = 0.95,
                     if_cenpoint = F){
    
    # Judge whether the length of [factor1] and the row numbers in [df_data] are equal to each other
    if(length(row.names(df_data)) != length(factor1)){
        stop(simpleError("Vectors [factor1] and the number of rows in [df_data] have different lengths"))
    }
    
    # Use function 'metaMDS()' to reduce dimensionality
    nmds_res = metaMDS(df_data, distance = distance, k = k, trymax = trymax)
    
    # Calculate the range of x axis
    mds1_max = max(nmds_res$points[, 1])
    mds1_min = min(nmds_res$points[, 1])
    xDelta = mds1_max - mds1_min
    xMax = mds1_max + (0.10 + x_margin) * xDelta
    xMin = mds1_min - (0.10 + x_margin) * xDelta
    
    # Calculate the range of y axis
    mds2_max = max(nmds_res$points[, 2])
    mds2_min = min(nmds_res$points[, 2])
    yDelta = mds2_max - mds2_min
    yMax = mds2_max + (0.10 + y_margin) * yDelta
    yMin = mds2_min - (0.10 + y_margin) * yDelta
    
    # Set point shapes and colors for factor1
    df_groups = setGroups(factor1 = factor1,
                          order_fac1 = order_fac1,
                          order_pch = order_pch, order_col = order_col)
    row.names(df_groups) = row.names(df_data)
    df_groups[, 'col'] = paste(df_groups[, 'col'], str_sub(rgb(0, 0, 0, point_alpha), 8, 9), sep = '')
    
    # Get unique groups, colors and shapes
    df_groups_unique = getGroups(df_groups = df_groups, order_fac1 = order_fac1)
    vec_fac1 = df_groups_unique[, 'Factor1']
    vec_col = df_groups_unique[, 'col']
    vec_pch = df_groups_unique[, 'pch']
    
    # Plot the layout
    plot(nmds_res, display = 'sites', type = 'n', xlab = '',  ylab = '', las = TRUE, choices = choices,
         xlim = c(xMin, xMax), ylim = c(yMin, yMax), cex.axis = 0.75 * text_cex)
    
    # Judge whether to draw a center cross
    if(if_cross){
        abline(v = 0, h = 0, col = rgb(0.4, 0.4, 0.4), lwd = 2.00, lty = 4)
    }
    
    # Set the line type for surround border
    if(is.null(sur_lty)){
        sur_lty = 1
    }
    
    # Draw surround
    drawSurround(ord = nmds_res, factor = df_groups[, 'Factor1'], colour = df_groups[, 'col'],
                 type = sur_type, if_link = sur_if_link, kind = sur_kind, conf = sur_conf,
                 lty = sur_lty, lwd = sur_lwd, alpha = sur_alpha)
    
    # Draw all points
    points(nmds_res$points[, c(choices[1], choices[2])],
           pch = df_groups$pch, col = point_edge_col, cex = 1.50 * point_cex, bg = df_groups[, 'col'])
    
    # Whether to draw the center points
    if(if_cenpoint){
        name_fac = 'Factor1'
        vec_fac_cp = vec_fac1
        vec_col_cp = vec_col
        vec_pch_cp = 21
        
        vec_cenpoint_x = c()
        vec_cenpoint_y = c()
        for(fac in vec_fac_cp){
            cenpoint_temp = colMeans(nmds_res$points[row.names(df_groups[df_groups[, name_fac] == fac, ]),
                                                     c(choices[1], choices[2])])
            vec_cenpoint_x = c(vec_cenpoint_x, cenpoint_temp[1])
            vec_cenpoint_y = c(vec_cenpoint_y, cenpoint_temp[2])
        }
        
        # Draw center points
        points(vec_cenpoint_x, vec_cenpoint_y, pch = vec_pch_cp, col = rgb(0, 0, 0, 1),
               bg = str_sub(vec_col_cp, 1, 7), cex = 2.25 * point_cex)
    }
    
    # Draw the arrows of environmental factors
    if(is.data.frame(df_envfit) | is.vector(df_envfit)){
        envfit_res = envfit(nmds_res, choices = choices, df_envfit, permutations = permutations)
        plot(envfit_res, choices = choices, p.max = p.max, col = 'black', cex = 1.00 * text_cex, font = 1)
    }
    
    # Use function 'adonis()' to determine the impact of these 2 factors on the community
    adonis_res = adonis(df_data ~ factor1, permutations = permutations)
    pValue1 = adonis_res$aov.tab$`Pr(>F)`[1]
    rSquare1 = adonis_res$aov.tab$R2[1]
    
    # Set the sub-title that contains information of stress, p-value and R-square
    tt = substitute(plain('Stress')~s*','
                    ~italic(p)*scriptscriptstyle(f1)~p1*','
                    ~plain(R)^2*scriptscriptstyle(f1)~r1,
                    list(s = cutOff(nmds_res$stress),
                         p1 = cutOff(pValue1), r1 = cutOff(rSquare1), f1 = label_fac1))
    
    # Draw main and sub title
    text(mean(c(xMax, xMin)), yMax + 0.20 * yDelta, labels = main, xpd = T, adj = 0.5, cex =  1.40 * text_cex, font = 2)
    text(mean(c(xMax, xMin)), yMax + 0.10 * yDelta, labels = tt, xpd = T, adj = 0.5, cex = 1.10 * text_cex)
    
    # Calculate the variation proportion of each axis
    vec_var_prop = round(proportions(apply(nmds_res$species, 2, var)) * 100, 2)
    
    # Write label for x axis
    if(is.null(xlab)){
        xlab = paste('NMDS ', choices[1], ' (', vec_var_prop[choices[1]], '% variation)', sep = '')
    }
    
    # Write label for y axis
    if(is.null(ylab)){
        ylab = paste('NMDS ', choices[2], ' (', vec_var_prop[choices[2]], '% variation)', sep = '')
    }
    
    # Draw x and y axis
    title(main = '', xlab = xlab, ylab = ylab, cex.lab = 1.20 * text_cex)
    
    # Draw legend
    if(if_legend){
        legend(pos_legend, legend = vec_fac1, pch = vec_pch, col = rgb(0, 0, 0, 1), pt.bg = str_sub(vec_col, 1, 7),
               cex = 1.00 * text_cex, pt.cex = 1.50 * point_cex, text.font = 1, bty = 'n')
    }
}

plotNMDS2 = function(df_data,
                     factor1, factor2,
                     order_fac1 = NULL, order_fac2 = NULL,
                     order_pch = NULL, order_col = NULL,
                     label_fac1 = '', label_fac2 = '',
                     distance = 'bray', k = 3, trymax = 1e4, choices = c(1, 2),
                     df_envfit = NULL, permutations = 1e4, p.max = 0.05,
                     text_cex = 1.00, main = 'Distribution of Data', xlab = NULL, ylab = NULL,
                     point_cex = 1.00, point_alpha = 1.00, point_edge_col = rgb(0, 0, 0, 1),
                     if_legend = T, pos_legend = 'topright',
                     if_cross = T, x_margin = 0.00, y_margin = 0.00,
                     sur_level = 1, sur_type = 'oval', sur_if_link = NULL,
                     sur_kind = 'sd', sur_lty = NULL, sur_lwd = 1, sur_alpha = 0.30, sur_conf = 0.95,
                     if_cenpoint = F){
    
    # Judge whether the length of [factor1], [factor2] and the row numbers in [df_data] are equal to each other
    if(T %in% (length(row.names(df_data)) != c(length(factor1), length(factor2)))){
        stop(simpleError("Vectors [factor1], [factor2] and the number of rows in [df_data] have different lengths"))
    }
    
    # Use function 'metaMDS()' to reduce dimensionality
    nmds_res = metaMDS(df_data, distance = distance, k = k, trymax = trymax)
    
    # Calculate the range of x axis
    mds1_max = max(nmds_res$points[, 1])
    mds1_min = min(nmds_res$points[, 1])
    xDelta = mds1_max - mds1_min
    xMax = mds1_max + (0.10 + x_margin) * xDelta
    xMin = mds1_min - (0.10 + x_margin) * xDelta
    
    # Calculate the range of y axis
    mds2_max = max(nmds_res$points[, 2])
    mds2_min = min(nmds_res$points[, 2])
    yDelta = mds2_max - mds2_min
    yMax = mds2_max + (0.10 + y_margin) * yDelta
    yMin = mds2_min - (0.10 + y_margin) * yDelta
    
    # Set point shapes and colors for factor1 and factor2
    df_groups = setGroups(factor1 = factor1, factor2 = factor2,
                          order_fac1 = order_fac1, order_fac2 = order_fac2,
                          order_pch = order_pch, order_col = order_col)
    row.names(df_groups) = row.names(df_data)
    df_groups[, 'col'] = paste(df_groups[, 'col'], str_sub(rgb(0, 0, 0, point_alpha), 8, 9), sep = '')
    
    # Get unique groups, colors and shapes
    df_groups_unique = getGroups(df_groups = df_groups, order_fac1 = order_fac1, order_fac2 = order_fac2)
    vec_fac1 = unique(df_groups_unique[, 'Factor1'])
    vec_fac2 = unique(df_groups_unique[, 'Factor2'])
    vec_facs = df_groups_unique[, 'Factors']
    vec_cols = df_groups_unique[, 'col']
    vec_col = unique(vec_cols)
    vec_pchs = df_groups_unique[, 'pch']
    vec_pch = unique(vec_pchs)
    
    # Plot the layout
    plot(nmds_res, display = 'sites', type = 'n', xlab = '',  ylab = '', las = TRUE, choices = choices,
         xlim = c(xMin, xMax), ylim = c(yMin, yMax), cex.axis = 0.75 * text_cex)
    
    # Judge whether to draw a center cross
    if(if_cross){
        abline(v = 0, h = 0, col = rgb(0.4, 0.4, 0.4), lwd = 2.00, lty = 4)
    }
    
    # Set the line type for surround border
    if(is.null(sur_lty)){
        sur_lty = 1
    }
    
    # Judge which level of surround to draw
    if(sur_level == 1){
        drawSurround(ord = nmds_res, factor = df_groups[, 'Factor1'], colour = df_groups[, 'col'],
                     type = sur_type, if_link = sur_if_link, kind = sur_kind, conf = sur_conf,
                     lty = sur_lty, lwd = sur_lwd, alpha = sur_alpha)
    }
    else if(sur_level == 2){
        drawSurround(ord = nmds_res, factor = df_groups[, 'Factors'], colour = df_groups[, 'col'],
                     type = sur_type, if_link = sur_if_link, kind = sur_kind, conf = sur_conf,
                     lty = sur_lty, lwd = sur_lwd, alpha = sur_alpha)
    }
    
    # Draw all points
    points(nmds_res$points[, c(choices[1], choices[2])],
           pch = df_groups$pch, col = point_edge_col, cex = 1.50 * point_cex, bg = df_groups[, 'col'])
    
    # Whether to draw the center points
    if(if_cenpoint){
        if(sur_level == 1){
            name_fac = 'Factor1'
            vec_fac_cp = vec_fac1
            vec_col_cp = vec_col
            vec_pch_cp = 21
            if_continue = T
        }
        else if(sur_level == 2){
            name_fac = 'Factors'
            vec_fac_cp = vec_facs
            vec_col_cp = vec_cols
            vec_pch_cp = vec_pchs
            if_continue = T
        }
        else{
            if_continue = F
        }
        
        if(if_continue){
            vec_cenpoint_x = c()
            vec_cenpoint_y = c()
            for(fac in vec_fac_cp){
                cenpoint_temp = colMeans(nmds_res$points[row.names(df_groups[df_groups[, name_fac] == fac, ]),
                                                         c(choices[1], choices[2])])
                vec_cenpoint_x = c(vec_cenpoint_x, cenpoint_temp[1])
                vec_cenpoint_y = c(vec_cenpoint_y, cenpoint_temp[2])
            }
            
            # Draw center points
            points(vec_cenpoint_x, vec_cenpoint_y, pch = vec_pch_cp, col = rgb(0, 0, 0, 1),
                   bg = str_sub(vec_col_cp, 1, 7), cex = 2.25 * point_cex)
        }
    }
    
    # Draw the arrows of environmental factors
    if(is.data.frame(df_envfit) | is.vector(df_envfit)){
        envfit_res = envfit(nmds_res, choices = choices, df_envfit, permutations = permutations)
        plot(envfit_res, choices = choices, p.max = p.max, col = 'black', cex = 1.00 * text_cex, font = 1)
    }
    
    # Use function 'adonis()' to determine the impact of these 2 factors on the community
    adonis_res = adonis(df_data ~ factor1 + factor2, permutations = permutations)
    pValue1 = adonis_res$aov.tab$`Pr(>F)`[1]
    pValue2 = adonis_res$aov.tab$`Pr(>F)`[2]
    rSquare1 = adonis_res$aov.tab$R2[1]
    rSquare2 = adonis_res$aov.tab$R2[2]
    
    # Set the sub-title that contains information of stress, p-value and R-square
    tt = substitute(plain('Stress')~s*','
                    ~italic(p)*scriptscriptstyle(f1)~p1*','
                    ~plain(R)^2*scriptscriptstyle(f1)~r1*','
                    ~italic(p)*scriptscriptstyle(f2)~p2
                    *','~plain(R)^2*scriptscriptstyle(f2)~r2,
                    list(s = cutOff(nmds_res$stress),
                         p1 = cutOff(pValue1), r1 = cutOff(rSquare1), f1 = label_fac1,
                         p2 = cutOff(pValue2), r2 = cutOff(rSquare2), f2 = label_fac2))
    
    # Draw main and sub title
    text(mean(c(xMax, xMin)), yMax + 0.20 * yDelta, labels = main, xpd = T, adj = 0.5, cex =  1.40 * text_cex, font = 2)
    text(mean(c(xMax, xMin)), yMax + 0.10 * yDelta, labels = tt, xpd = T, adj = 0.5, cex = 1.10 * text_cex)
    
    # Calculate the variation proportion of each axis
    vec_var_prop = round(proportions(apply(nmds_res$species, 2, var)) * 100, 2)
    
    # Write label for x axis
    if(is.null(xlab)){
        xlab = paste('NMDS ', choices[1], ' (', vec_var_prop[choices[1]], '% variation)', sep = '')
    }
    
    # Write label for y axis
    if(is.null(ylab)){
        ylab = paste('NMDS ', choices[2], ' (', vec_var_prop[choices[2]], '% variation)', sep = '')
    }
    
    # Draw x and y axis
    title(main = '', xlab = xlab, ylab = ylab, cex.lab = 1.20 * text_cex)
    
    # Draw legend
    if(if_legend){
        legend(pos_legend, legend = vec_facs, pch = vec_pchs, col = rgb(0, 0, 0, 1), pt.bg = str_sub(vec_cols, 1, 7),
               cex = 1.00 * text_cex, pt.cex = 1.50 * point_cex, text.font = 1, bty = 'n')
    }
}

plotNMDS = function(df_data,
                    factor1 = NULL, factor2 = NULL,
                    order_fac1 = NULL, order_fac2 = NULL,
                    order_pch = NULL, order_col = NULL,
                    label_fac1 = '', label_fac2 = '',
                    distance = 'bray', k = 3, trymax = 1e4, choices = c(1, 2),
                    df_envfit = NULL, permutations = 1e4, p.max = 0.05,
                    text_cex = 1.00, main = 'Distribution of Data', xlab = NULL, ylab = NULL,
                    point_cex = 1.00, point_alpha = 1.00, point_edge_col = rgb(0, 0, 0, 1),
                    if_legend = T, pos_legend = 'topright',
                    if_cross = T, x_margin = 0.00, y_margin = 0.00,
                    sur_level = 1, sur_type = 'oval', sur_if_link = NULL,
                    sur_kind = 'sd', sur_lty = NULL, sur_lwd = 1, sur_alpha = 0.30, sur_conf = 0.95,
                    if_cenpoint = F){
    
    if(is.null(factor1) & is.null(factor2)){
        return(plotNMDS0(df_data,
                         21, rgb(0.5, 0.5, 0.5),
                         distance, k, trymax, choices,
                         df_envfit, permutations, p.max,
                         text_cex, main, xlab, ylab,
                         point_cex, point_alpha, point_edge_col,
                         if_cross, x_margin, y_margin,
                         sur_type, sur_if_link,
                         sur_kind, sur_lty, sur_lwd, sur_alpha, sur_conf,
                         if_cenpoint))
    }
    
    if(!is.null(factor1) & is.null(factor2)){
        return(plotNMDS1(df_data,
                         factor1,
                         order_fac1,
                         order_pch, order_col,
                         label_fac1,
                         distance, k, trymax, choices,
                         df_envfit, permutations, p.max,
                         text_cex, main, xlab, ylab,
                         point_cex, point_alpha, point_edge_col,
                         if_legend, pos_legend,
                         if_cross, x_margin, y_margin,
                         sur_type, sur_if_link,
                         sur_kind, sur_lty, sur_lwd, sur_alpha, sur_conf,
                         if_cenpoint))
    }
    
    if(!is.null(factor1) & !is.null(factor2)){
        return(plotNMDS2(df_data,
                         factor1, factor2,
                         order_fac1, order_fac2,
                         order_pch, order_col,
                         label_fac1, label_fac2,
                         distance, k, trymax, choices,
                         df_envfit, permutations, p.max,
                         text_cex, main, xlab, ylab,
                         point_cex, point_alpha, point_edge_col,
                         if_legend, pos_legend,
                         if_cross, x_margin, y_margin,
                         sur_level, sur_type, sur_if_link,
                         sur_kind, sur_lty, sur_lwd, sur_alpha, sur_conf,
                         if_cenpoint))
    }
    
    stop(simpleError("Wrong input! please check again."))
}

# Calculate the vertex position of the largest convex polygon
getPolygon = function(df){
    
    df = as.data.frame(df)
    vec_rownames = row.names(df)
    vertex_start = row.names(df[df[, 2] == min(df[, 2]), ])
    vec_vertex = c(vertex_start)
    vertex_this = vertex_start
    pos = 1
    ang_pre = 2
    while(pos == 1 | vertex_start != vertex_this){
        #vec_vertex = c(vec_vertex, vertex_this)
        ang_max = -2
        vertex_temp = ''
        
        if(pos == 1){
            vec_vertex_next = setdiff(vec_rownames, vec_vertex)
        }
        else{
            vec_vertex_next = c(setdiff(vec_rownames, vec_vertex), vertex_start)
        }
        
        for(each in vec_vertex_next){
            vec_temp = c(df[each, 1] - df[vertex_this, 1], df[each, 2] - df[vertex_this, 2])
            if(vec_temp[2] != 0){
                ang = vec_temp[2] / abs(vec_temp[2]) * (vec_temp[1] / sqrt(vec_temp[1] ^ 2 + vec_temp[2] ^ 2) + 1)
            }
            else{
                ang = 0
            }
            
            if(ang > ang_max & ang < ang_pre){
                ang_max = ang
                vertex_temp = each
            }
        }
        
        vertex_this = vertex_temp
        if(vertex_this != vertex_start){
            vec_vertex = c(vec_vertex, vertex_this)
        }
        ang_pre = ang_max
        pos = pos + 1
    }
    
    return(vec_vertex)
}

# Calculate correlation coeffient
calcCorr = function(df1, df2 = NULL, method = 'pearson'){
    
    # Judge whether the type of [df1] is 'data.frame' or 'matrix'
    if(!is.data.frame(df1) & !is.matrix(df1)){
        stop(simpleError("Wrong type of [df1], only 'data.frame' and 'matrix' are available"))
    }
    
    # If there is no [df2], then calculate the self-correlation of [df1]
    if(is.null(df2)){
        df2 = df1
    }
    
    # Judge whether the type of [df2] is 'data.frame' or 'matrix'
    if(!is.data.frame(df2) & !is.matrix(df2)){
        stop(simpleError("Wrong type of [df2], only 'data.frame' and 'matrix' are available"))
    }
    
    # Get the numbers of rows and columns in [df1] and [df2]
    n_row1 = nrow(df1)
    n_column1 = ncol(df1)
    n_row2 = nrow(df2)
    n_column2 = ncol(df2)
    
    # Judge whether the numbers of rows in [df1] and [df2] are equal to each other
    if(n_row1 != n_row2){
        stop(simpleError("The numbers of rows in [df1] and [df2] are different"))
    }
    
    vec_cor = c()
    vec_p = c()
    
    # Take indices in [df1] as the row names of correlation matrix
    for(ncol in c(1 : n_column1)){
        # Take indices in [df2] as the column names of correlation matrix
        for(nrow in c(1 : n_column2)){
            res_cor_temp = cor.test(df2[, nrow], df1[, ncol], alternative = 'two.sided', method = method)
            if(is.na(res_cor_temp$p.value)){
                warning(paste("Get nothing in the correlation calculation between '",
                              colnames(df1)[n_column1], "' and '", colnames(df2)[n_column2], "'!", sep = ''))
                res_cor_temp$estimate = 0
                res_cor_temp$p.value = 1
            }
            vec_cor = c(vec_cor, res_cor_temp$estimate)
            vec_p = c(vec_p, res_cor_temp$p.value)
        }
    }
    
    # Create a correlation coefficient matrix
    mat_cor = matrix(vec_cor, nrow = n_column1, ncol = n_column2, byrow = T)
    row.names(mat_cor) = colnames(df1)
    colnames(mat_cor) = colnames(df2)
    
    # Create a p-value matrix
    mat_p = matrix(vec_p, nrow = n_column1, ncol = n_column2, byrow = T)
    row.names(mat_p) = colnames(df1)
    colnames(mat_p) = colnames(df2)
    
    # Create a list to store these 2 results and return
    lst_res = list(mat_cor, mat_p)
    names(lst_res) = c('cor.coef', 'p.value')
    
    return(lst_res)
}

# Sort by order
sortByOrder = function(factor, ord){
    
    # Judge whether the elements in [factor] and [ord] are equal
    if(!setequal(factor, ord)){
        stop(simpleError("The unique elements in [factor] are incompitable with [ord]"))
    }
    
    vec_pos = c()
    for(each in ord){
        vec_pos = c(vec_pos, which(factor == each))
    }
    
    return(vec_pos)
}

# Plot heatmap
plotHeatmap = function(df1, df2 = NULL, hm_type = 'correlation',
                       gradient_col = NULL,
                       cell_width = 2, cell_height = 1,
                       text_cex = 1, text_font = 1,
                       main = '',
                       df_ann_row = NULL, df_ann_column = NULL,
                       order_ann_row = NULL, order_ann_column = NULL,
                       lst_ann_col = NULL, border_col = rgb(153, 153, 153, maxColorValue = 255),
                       corr_method = 'pearson'){
    
    vec_col_ann_row = NULL
    vec_col_ann_column = NULL
    
    if(!is.null(df_ann_row)){
        name_ann_row = colnames(df_ann_row)[1]
        factor_row = df_ann_row[, name_ann_row]
        
        if(!is.null(order_ann_row)){
            if(!(setequal(factor_row, order_ann_row))){
                stop(simpleError("The unique elements in [df_ann_row] are incompitable with [order_ann_row]"))
            }
            
            df_ann_row[, 'Temp'] = c(1 : ncol(df_ann_row))
            df_ann_row = df_ann_row[sortByOrder(factor_row, order_ann_row), ][name_ann_row]
        }
        
        fac_row = unique(df_ann_row[, name_ann_row])
        vec_col_ann_row = colorRampPalette(c(rgb(247, 162, 119, maxColorValue = 255),
                                             rgb(185, 210, 153, maxColorValue = 255)))(length(fac_row))
        names(vec_col_ann_row) = fac_row
    }
    
    if(!is.null(df_ann_column)){
        name_ann_column = colnames(df_ann_column)[1]
        factor_column = df_ann_column[, name_ann_column]
        
        if(!is.null(order_ann_column)){
            if(!(setequal(factor_column, order_ann_column))){
                stop(simpleError("The unique elements in [df_ann_column] are incompitable with [order_ann_column]"))
            }
            
            df_ann_column[, 'Temp'] = c(1 : nrow(df_ann_column))
            df_ann_column = df_ann_column[sortByOrder(factor_column, order_ann_column), ][name_ann_column]
        }
        
        fac_column = unique(df_ann_column[, name_ann_column])
        vec_col_ann_column = colorRampPalette(c(rgb(247, 162, 119, maxColorValue = 255),
                                                rgb(185, 210, 153, maxColorValue = 255)))(length(fac_column))
        names(vec_col_ann_column) = fac_column
    }
    
    if(is.null(lst_ann_col)){
        lst_ann_col = list()
        
        if(!is.null(df_ann_row)){
            lst_ann_col[[name_ann_row]] = vec_col_ann_row
        }
        
        if(!is.null(df_ann_column)){
            lst_ann_col[[name_ann_column]] = vec_col_ann_column
        }
        
        if(length(lst_ann_col) == 0){
            lst_ann_col = NULL
        }
    }
    
    if(hm_type == 'amount'){
        if(is.null(gradient_col)){
            gradient_col = colorRampPalette(c(rgb(204, 204, 204, maxColorValue = 255),
                                              rgb(228, 156, 170, maxColorValue = 255),
                                              rgb(197, 101, 119, maxColorValue = 255)))(1000)
        }
        
        df_hm = df1
        df_pValue = F
    }
    else if(hm_type == 'correlation'){
        if(is.null(gradient_col)){
            gradient_col = colorRampPalette(c(rgb(60, 132, 173, maxColorValue = 255),
                                              rgb(130, 190, 203, maxColorValue = 255),
                                              rgb(204, 204, 204, maxColorValue = 255),
                                              rgb(228, 156, 170, maxColorValue = 255),
                                              rgb(197, 101, 119, maxColorValue = 255)))(1000)
        }
        
        n_gradient_col = length(gradient_col)
        
        if(is.null(df2)){
            res_corr = calcCorr(df1, method = corr_method)
        }
        else{
            res_corr = calcCorr(df1, df2, method = corr_method)
        }
        
        df_hm = res_corr$cor.coef
        df_pValue = apply(res_corr$p.value, 2, calcAsterisk)
        
        corr_min = min(df_hm)
        corr_max = max(df_hm)
        
        if(corr_min > 0){
            cut_point = floor(0.5 * n_gradient_col)
            gradient_col = gradient_col[c(cut_point : n_gradient_col)]
        }
        else if(corr_max < 0){
            cut_point = ceiling(0.5 * n_gradient_col)
            gradient_col = gradient_col[c(1 : cut_point)]
        }
        else if(abs(corr_min) < corr_max){
            cut_point = floor((1 + corr_min / corr_max) * n_gradient_col / 2)
            gradient_col = gradient_col[c(cut_point : n_gradient_col)]
        }
        else{
            cut_point = floor(n_gradient_col / 2 * (1 - corr_max / corr_min))
            gradient_col = gradient_col[c(1 : cut_point)]
        }
    }
    else{
        stop(simpleError("Wrong input of [hm_type], only 'correlation' and 'amount' are available"))
    }
    
    n_hm_row = nrow(df_hm)
    n_hm_column = ncol(df_hm)
    
    if(n_hm_row > 1 & n_hm_column > 1){
        if(!is.null(df_ann_row)){
            df_hm = df_hm[row.names(df_ann_row), ]
        }
        
        if(!is.null(df_ann_column)){
            df_hm = df_hm[, row.names(df_ann_column)]
        }
    }
    else if(n_hm_row == 1 & n_hm_column > 1){
        if(!is.null(df_ann_column)){
            df_hm['Temp', ] = c(1 : n_hm_column)
            df_hm = df_hm[name_ann_row, row.names(df_ann_column)]
        }
    }
    else if(n_hm_row > 1 & n_hm_column == 1){
        if(!is.null(df_ann_column)){
            df_hm[, 'Temp'] = c(1 : n_hm_row)
            df_hm = df_hm[row.names(df_ann_column), name_ann_column]
        }
    }
    
    pheatmap(df_hm, col = gradient_col,
             cellwidth = 10 * cell_width, cellheight = 10 * cell_height,
             fontsize_number = 12 * text_cex, fontsize = 8 * text_cex,
             display_numbers = df_pValue,
             cluster_cols = F, cluster_rows = F,
             main = main,
             annotation_row = df_ann_row,
             annotation_col = df_ann_column,
             annotation_colors = lst_ann_col,
             border_color = border_col)
}
