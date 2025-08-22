import { writeFile, mkdir } from 'fs/promises'
import { dirname } from 'path'
import { ErrorCategory, PluginError } from '@utils/error'

export class FileWriter {
	async write(content: string, outputPath: string, contentType = 'file'): Promise<void> {
		try {
			const dir = dirname(outputPath)
			await mkdir(dir, { recursive: true })
			await writeFile(outputPath, content, 'utf8')
		} catch (error) {
			if (error instanceof Error) {
				throw new PluginError(
					`Failed to write ${contentType} to ${outputPath}: ${error.message}`,
					ErrorCategory.FILE,
					{ outputPath, error: error.message },
					['Check if the output directory exists and is writable', 'Verify the output path is valid'],
				)
			}
			throw new PluginError(`Failed to write ${contentType} to ${outputPath}`, ErrorCategory.FILE, { outputPath }, [
				'Check if the output directory exists and is writable',
				'Verify the output path is valid',
			])
		}
	}
}
