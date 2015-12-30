
% system('mogrify -resize 1067 *.jpg')


dirs = dir('images');
disp('START SCALING IMAGES');

for directory=dirs'
    %directory.name
    dir_name = directory.name;
    dir_path = sprintf('images/%s', directory.name);
    
    if isdir(dir_path) && ~strcmp(dir_name, '.') && ~strcmp(dir_name, '..')
        
        format = 'jpg';
        
        fprintf('----- Processing directory "%s" -----\n', dir_name);
        
        for fileobj=dir(dir_path)'
            filepath = sprintf('%s/%s', dir_path, fileobj.name);
            [~, ~, file_extension] = fileparts(filepath);
            if ~isdir(filepath) && strcmpi(file_extension, '.jpg')
                img = imread(filepath);
                scaled = imresize(img, [800, NaN]);
                imwrite(scaled, filepath);
            end;
        end;
            

        
    end;
end;
