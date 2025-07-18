import { promises as fs } from 'fs'
import { dirname, resolve, isAbsolute } from 'path'
import { ErrorHandler, FileError } from './error-handler'

export interface WriteOptions {
	createDirectories?: boolean
	overwrite?: boolean
	encoding?: BufferEncoding
	backup?: boolean
}

export interface WriteResult {
	success: boolean
	path: string
	bytesWritten: number
	backupPath?: string
}

export class FileWriter {
	private errorHandler: ErrorHandler

	constructor(errorHandler: ErrorHandler = ErrorHandler.getInstance()) {
		this.errorHandler = errorHandler
	}

	async writeSchema(content: string, outputPath: string, options: WriteOptions = {}): Promise<WriteResult> {
		const resolvedPath = this.resolvePath(outputPath)
		const finalOptions = this.normalizeOptions(options)

		try {
			await this.validateWriteOperation(resolvedPath, finalOptions)

			let backupPath: string | undefined
			if (finalOptions.backup && (await this.fileExists(resolvedPath))) {
				backupPath = await this.createBackup(resolvedPath)
			}

			if (finalOptions.createDirectories) {
				await this.ensureDirectoryExists(dirname(resolvedPath))
			}

			await fs.writeFile(resolvedPath, content, { encoding: finalOptions.encoding })
			const stats = await fs.stat(resolvedPath)

			return {
				success: true,
				path: resolvedPath,
				bytesWritten: stats.size,
				backupPath,
			}
		} catch (error) {
			return this.handleWriteError(error, resolvedPath, content, finalOptions)
		}
	}

	async writeMultipleFiles(files: Array<{ path: string; content: string }>, options: WriteOptions = {}): Promise<WriteResult[]> {
		const results: WriteResult[] = []
		const finalOptions = this.normalizeOptions(options)

		for (const file of files) {
			try {
				const result = await this.writeSchema(file.content, file.path, finalOptions)
				results.push(result)
			} catch (error) {
				const failedResult = this.createFailedResult(file.path, error)
				results.push(failedResult)
			}
		}

		return results
	}

	async ensureDirectoryExists(dirPath: string): Promise<void> {
		try {
			await fs.access(dirPath)
		} catch {
			try {
				await fs.mkdir(dirPath, { recursive: true })
			} catch (error) {
				throw this.errorHandler.createFileError(`Failed to create directory: ${dirPath}`, { directory: dirPath, operation: 'mkdir' }, [
					'Check directory permissions',
					'Ensure parent directories are writable',
					'Verify disk space availability',
				])
			}
		}
	}

	async fileExists(filePath: string): Promise<boolean> {
		try {
			await fs.access(filePath)
			return true
		} catch {
			return false
		}
	}

	async createBackup(filePath: string): Promise<string> {
		const timestamp = new Date().toISOString().replace(/[:.]/g, '-')
		const backupPath = `${filePath}.backup.${timestamp}`

		try {
			await fs.copyFile(filePath, backupPath)
			return backupPath
		} catch (error) {
			throw this.errorHandler.createFileError(`Failed to create backup of ${filePath}`, { originalPath: filePath, backupPath, operation: 'backup' }, [
				'Check file permissions',
				'Ensure sufficient disk space',
				'Verify source file exists and is readable',
			])
		}
	}

	private resolvePath(outputPath: string): string {
		if (!outputPath || outputPath.trim() === '') {
			throw this.errorHandler.createFileError('Output path cannot be empty', { providedPath: outputPath }, ['Provide a valid file path'])
		}

		return isAbsolute(outputPath) ? outputPath : resolve(process.cwd(), outputPath)
	}

	private normalizeOptions(options: WriteOptions): Required<WriteOptions> {
		return {
			createDirectories: options.createDirectories ?? true,
			overwrite: options.overwrite ?? true,
			encoding: options.encoding ?? 'utf8',
			backup: options.backup ?? false,
		}
	}

	private async validateWriteOperation(filePath: string, options: Required<WriteOptions>): Promise<void> {
		const fileExists = await this.fileExists(filePath)

		if (fileExists && !options.overwrite) {
			throw this.errorHandler.createFileError(`File already exists and overwrite is disabled: ${filePath}`, { filePath, overwrite: options.overwrite }, [
				'Set overwrite option to true',
				'Choose a different output path',
				'Remove the existing file manually',
			])
		}

		const directory = dirname(filePath)
		const directoryExists = await this.fileExists(directory)

		if (!directoryExists && !options.createDirectories) {
			throw this.errorHandler.createFileError(`Directory does not exist and createDirectories is disabled: ${directory}`, { directory, createDirectories: options.createDirectories }, [
				'Set createDirectories option to true',
				'Create the directory manually',
				'Use an existing directory path',
			])
		}
	}

	private async handleWriteError(error: unknown, filePath: string, content: string, options: Required<WriteOptions>): Promise<WriteResult> {
		const fallbackResult = await this.attemptFallbackWrite(filePath, content, options)

		if (fallbackResult.success) {
			return fallbackResult
		}

		throw this.errorHandler.createFileError(
			`Failed to write file: ${filePath}`,
			{
				filePath,
				contentLength: content.length,
				options,
				originalError: error instanceof Error ? error.message : String(error),
			},
			['Check file and directory permissions', 'Ensure sufficient disk space', 'Verify the path is valid and accessible', 'Try writing to a different location']
		)
	}

	private async attemptFallbackWrite(originalPath: string, content: string, options: Required<WriteOptions>): Promise<WriteResult> {
		const fallbackPaths = this.generateFallbackPaths(originalPath)

		for (const fallbackPath of fallbackPaths) {
			try {
				if (options.createDirectories) {
					await this.ensureDirectoryExists(dirname(fallbackPath))
				}

				await fs.writeFile(fallbackPath, content, { encoding: options.encoding })
				const stats = await fs.stat(fallbackPath)

				return {
					success: true,
					path: fallbackPath,
					bytesWritten: stats.size,
				}
			} catch {
				continue
			}
		}

		return this.createFailedResult(originalPath, new Error('All fallback attempts failed'))
	}

	private generateFallbackPaths(originalPath: string): string[] {
		const dir = dirname(originalPath)
		const ext = originalPath.split('.').pop() || ''
		const baseName = originalPath.replace(`.${ext}`, '')
		const timestamp = Date.now()

		return [`${baseName}.fallback.${ext}`, `${baseName}.${timestamp}.${ext}`, resolve(process.cwd(), `fallback-output.${ext}`), resolve(process.cwd(), `output-${timestamp}.${ext}`)]
	}

	private createFailedResult(filePath: string, error: unknown): WriteResult {
		return {
			success: false,
			path: filePath,
			bytesWritten: 0,
		}
	}

	static create(): FileWriter {
		return new FileWriter()
	}
}

export default FileWriter.create()
