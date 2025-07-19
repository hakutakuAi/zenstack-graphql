import { promises as fs } from 'fs'
import { dirname, resolve, isAbsolute } from 'path'
import { Result, ok, err } from 'neverthrow'
import { ErrorCategory, PluginErrorData, createError } from '@utils/error'

export interface WriteOptions {
	createDirectories?: boolean
	overwrite?: boolean
	encoding?: BufferEncoding
	backup?: boolean
}

export interface WriteResult {
	path: string
	bytesWritten: number
	backupPath?: string
}

export class FileWriter {
	async writeSchema(content: string, outputPath: string, options: WriteOptions = {}): Promise<Result<WriteResult, PluginErrorData>> {
		try {
			const resolvedPathResult = this.resolvePath(outputPath)
			if (resolvedPathResult.isErr()) {
				return err(resolvedPathResult.error)
			}
			const resolvedPath = resolvedPathResult.value
			const finalOptions = this.normalizeOptions(options)

			const validateResult = await this.validateWriteOperation(resolvedPath, finalOptions)
			if (validateResult.isErr()) {
				return err(validateResult.error)
			}

			let backupPath: string | undefined
			if (finalOptions.backup && (await this.fileExists(resolvedPath))) {
				const backupResult = await this.createBackup(resolvedPath)
				if (backupResult.isErr()) {
					return err(backupResult.error)
				}
				backupPath = backupResult.value
			}

			if (finalOptions.createDirectories) {
				const dirResult = await this.ensureDirectoryExists(dirname(resolvedPath))
				if (dirResult.isErr()) {
					return err(dirResult.error)
				}
			}

			await fs.writeFile(resolvedPath, content, { encoding: finalOptions.encoding })
			const stats = await fs.stat(resolvedPath)

			return ok({
				path: resolvedPath,
				bytesWritten: stats.size,
				backupPath,
			})
		} catch (error) {
			return createError(`Failed to write file: ${outputPath}`, ErrorCategory.FILE, { path: outputPath, error })
		}
	}

	async writeMultipleFiles(files: Array<{ path: string; content: string }>, options: WriteOptions = {}): Promise<Result<WriteResult[], PluginErrorData>> {
		const results: WriteResult[] = []
		const finalOptions = this.normalizeOptions(options)

		for (const file of files) {
			const result = await this.writeSchema(file.content, file.path, finalOptions)
			if (result.isErr()) {
				return err(result.error)
			}
			results.push(result.value)
		}

		return ok(results)
	}

	async ensureDirectoryExists(dirPath: string): Promise<Result<void, PluginErrorData>> {
		const directoryExists = await this.directoryExistsInternal(dirPath)
		if (!directoryExists) {
			try {
				await fs.mkdir(dirPath, { recursive: true })
				return ok(undefined)
			} catch (error) {
				return createError(`Failed to create directory: ${dirPath}`, ErrorCategory.FILE, { path: dirPath, error })
			}
		}
		return ok(undefined)
	}

	private async directoryExistsInternal(dirPath: string): Promise<boolean> {
		try {
			await fs.access(dirPath)
			return true
		} catch {
			return false
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

	async createBackup(filePath: string): Promise<Result<string, PluginErrorData>> {
		try {
			const timestamp = new Date().toISOString().replace(/[:.]/g, '-')
			const backupPath = `${filePath}.backup.${timestamp}`

			await fs.copyFile(filePath, backupPath)
			return ok(backupPath)
		} catch (error) {
			return createError(`Failed to create backup of file: ${filePath}`, ErrorCategory.FILE, { path: filePath, error })
		}
	}

	private resolvePath(outputPath: string): Result<string, PluginErrorData> {
		if (!outputPath || outputPath.trim() === '') {
			return createError('Output path cannot be empty', ErrorCategory.FILE, { outputPath })
		}

		return ok(isAbsolute(outputPath) ? outputPath : resolve(process.cwd(), outputPath))
	}

	private normalizeOptions(options: WriteOptions): Required<WriteOptions> {
		return {
			createDirectories: options.createDirectories ?? true,
			overwrite: options.overwrite ?? true,
			encoding: options.encoding ?? 'utf8',
			backup: options.backup ?? false,
		}
	}

	private async validateWriteOperation(filePath: string, options: Required<WriteOptions>): Promise<Result<void, PluginErrorData>> {
		const fileExists = await this.fileExists(filePath)

		if (fileExists && !options.overwrite) {
			return createError(`File already exists and overwrite is disabled: ${filePath}`, ErrorCategory.FILE, { filePath, overwrite: options.overwrite }, [
				'Set overwrite option to true',
				'Choose a different output path',
				'Remove the existing file manually',
			])
		}

		const directory = dirname(filePath)
		const directoryExists = await this.directoryExistsInternal(directory)

		if (!directoryExists && !options.createDirectories) {
			return createError(`Directory does not exist and createDirectories is disabled: ${directory}`, ErrorCategory.FILE, { directory, createDirectories: options.createDirectories }, [
				'Set createDirectories option to true',
				'Create the directory manually',
				'Use an existing directory path',
			])
		}

		return ok(undefined)
	}

	static create(): FileWriter {
		return new FileWriter()
	}
}
